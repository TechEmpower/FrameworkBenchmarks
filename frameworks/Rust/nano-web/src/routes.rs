use crate::compression::CompressedContent;
use crate::mime_types::{get_cache_control, get_mime_config};
use crate::template::render_template;
use anyhow::Result;
use dashmap::DashMap;
use fxhash::FxBuildHasher;
use memmap2::Mmap;
use rayon::prelude::*;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;
use tracing::{debug, error, info};
use walkdir::WalkDir;

#[derive(Debug, Clone)]
pub struct CachedRoute {
    // Arc<CompressedContent> - ACTUAL SHARING: When index.html files create duplicate routes
    // (both "/about/index.html" and "/about/" point to same file), Arc prevents copying
    // the compressed content. See lines 133-138 where route.clone() shares the Arc data
    pub content: Arc<CompressedContent>,

    // Arc<PathBuf> - Same index.html sharing as above. Also enables cheap cloning when
    // CachedRoute is returned from get_route() (line 203) without copying path strings
    pub path: Arc<PathBuf>,

    // SystemTime is just a timestamp (u64) - cheap to copy, no Arc needed
    // Would be wasteful to add Arc overhead for such a small value
    pub modified: SystemTime,

    // Arc<CachedRouteHeaders> - Enables cheap cloning when returning routes from cache.
    // IMPORTANT: Headers are NOT shared between different files currently (each file
    // gets its own Arc::new(CachedRouteHeaders {...})). The Arc is for clone performance
    pub headers: Arc<CachedRouteHeaders>,
}

#[derive(Debug, Clone)]
pub struct CachedRouteHeaders {
    // Arc<str> vs String vs Arc<String> - WHY THIS SPECIFIC TYPE:
    //
    // String: Owned, mutable, heap-allocated (grows/shrinks)
    // Arc<String>: Shared String, but String itself has extra heap allocation overhead
    // Arc<str>: Shared string slice, more compact - no separate heap allocation for string data
    //
    // ACTUAL USAGE: Currently each file creates its own header strings (no sharing between
    // files). Arc<str> is used here for efficient cloning when CachedRouteHeaders is cloned,
    // and because Arc<str> is more memory-efficient than String for immutable data.
    pub content_type: Arc<str>, // Arc::from(mime_config.mime_type.as_str()) - each file gets its own
    pub last_modified: Arc<str>, // Arc::from(last_modified.as_str()) - unique per file timestamp
    pub etag: Arc<str>,         // Arc::from(etag.as_str()) - unique per file content+timestamp
    pub cache_control: Arc<str>, // Arc::from(get_cache_control(...)) - could potentially be shared
}

// Type alias for a concurrent hash map. DashMap allows lock-free concurrent access.
//
// Arc<str> keys - CRITICAL CHOICE: URL paths like "/assets/app.js" are stored once
// and shared. Without Arc, each HashMap key would own its own copy of the path string.
// With Arc, the same path string is shared between the key and the CachedRoute.path
//
// FxBuildHasher - trades cryptographic security for 2x faster hashing on strings
// Safe because we control the keys (URL paths), no hash-flooding attacks possible
pub type CachedRoutes = DashMap<Arc<str>, CachedRoute, FxBuildHasher>;

pub struct NanoWeb {
    // The main route cache using our type alias defined above
    pub routes: CachedRoutes,

    // Memory-mapped files cache. Mmap provides zero-copy file access by mapping
    // file contents directly into memory.
    //
    // Arc<Mmap> - ESSENTIAL: Mmap maps file contents into process memory. Without Arc,
    // we couldn't share this mapped memory across threads safely. Arc ensures the
    // memory mapping stays valid as long as any thread holds a reference.
    // Multiple requests for the same large file can serve from the same mapped memory.
    pub static_cache: DashMap<Arc<str>, Arc<Mmap>, FxBuildHasher>,
}

impl Default for NanoWeb {
    fn default() -> Self {
        Self::new()
    }
}

impl NanoWeb {
    pub fn new() -> Self {
        Self {
            routes: DashMap::with_hasher(FxBuildHasher::default()),
            static_cache: DashMap::with_hasher(FxBuildHasher::default()),
        }
    }

    pub fn populate_routes(&self, public_dir: &Path, config_prefix: &str) -> Result<()> {
        debug!("Starting route population from {:?}", public_dir);

        // Collect all file paths first
        // Vec<_> uses type inference - the compiler figures out the full type from context
        let file_paths: Vec<_> = WalkDir::new(public_dir)
            .into_iter()
            .filter_map(|entry| {
                // Option::ok() converts Result<T,E> to Option<T>, discarding errors
                entry.ok().and_then(|e| {
                    if e.file_type().is_file() {
                        // PathBuf::to_path_buf() creates owned path data we can move around
                        // The ? operator propagates None if metadata() fails
                        Some((e.path().to_path_buf(), e.metadata().ok()?))
                    } else {
                        None
                    }
                })
            })
            .collect();

        info!("Processing {} files in parallel", file_paths.len());

        // Process files in parallel using rayon's parallel iterator
        // par_iter() automatically distributes work across CPU cores
        let routes: Vec<_> = file_paths
            .par_iter()
            .filter_map(|(file_path, metadata)| {
                match self.create_route(file_path, metadata, public_dir, config_prefix) {
                    Ok((url_path, route)) => Some((url_path, route)),
                    Err(e) => {
                        error!("Failed to create route for {:?}: {}", file_path, e);
                        None
                    }
                }
            })
            .collect();

        // Insert routes into concurrent map
        for (url_path, route) in routes {
            // Handle index files first - create the directory route
            if url_path.ends_with("/index.html") {
                let dir_path = if url_path.as_ref() == "/index.html" {
                    Arc::from("/")
                } else {
                    let dir = url_path.trim_end_matches("/index.html");
                    Arc::from(format!("{}/", dir))
                };
                // Clone for the directory route (e.g., "/about/" -> route)
                self.routes.insert(dir_path, route.clone());
            }

            // Insert the main route, moving ownership (no clone needed)
            // e.g., "/about/index.html" -> route (moved, no clone)
            self.routes.insert(url_path, route);
        }

        info!("Routes populated: {} routes", self.routes.len());
        Ok(())
    }

    // WHY THIS ISN'T CachedRoute::new() - This method needs access to NanoWeb's static_cache
    // to store memory-mapped files (line 150). It also needs self.file_path_to_url() and
    // self.generate_etag() methods. Moving this to CachedRoute would require passing
    // the cache and utility methods, making the API more complex.
    fn create_route(
        &self,
        file_path: &Path,
        metadata: &std::fs::Metadata,
        public_dir: &Path,
        config_prefix: &str,
    ) -> Result<(Arc<str>, CachedRoute)> {
        // Memory-map large files for zero-copy serving
        // 8192 bytes (8KB) threshold - small files stay in RAM, large files use mmap
        let content = if metadata.len() > 8192 {
            // Use memory mapping for larger files
            let file = File::open(file_path)?;
            // unsafe required because mmap can access arbitrary memory if file is modified
            // externally. Safe here because we control the file system
            let mmap = unsafe { Mmap::map(&file)? };
            let url_path = self.file_path_to_url(file_path, public_dir)?;
            self.static_cache.insert(url_path.clone(), Arc::new(mmap));

            // For mmap files, we still need a copy for compression
            // Vec<u8> contains the raw bytes of the file
            std::fs::read(file_path)?
        } else {
            std::fs::read(file_path)?
        };

        let modified = metadata.modified()?;
        let mime_config = get_mime_config(file_path);

        // Apply templating if needed
        let processed_content = if mime_config.is_templatable {
            match render_template(&String::from_utf8_lossy(&content), config_prefix) {
                Ok(templated) => templated.into_bytes(),
                Err(e) => {
                    error!("Template rendering failed for {:?}: {}", file_path, e);
                    content
                }
            }
        } else {
            content
        };

        let compressed = CompressedContent::new(processed_content, mime_config.is_compressible)?;
        let etag = self.generate_etag(&modified, &compressed.plain);
        let last_modified = self.format_http_date(modified);

        let headers = Arc::new(CachedRouteHeaders {
            content_type: Arc::from(mime_config.mime_type.as_str()),
            last_modified: Arc::from(last_modified.as_str()),
            etag: Arc::from(etag.as_str()),
            cache_control: Arc::from(get_cache_control(&mime_config.mime_type)),
        });

        let route = CachedRoute {
            content: Arc::new(compressed),
            path: Arc::new(file_path.to_path_buf()),
            modified,
            headers,
        };

        let url_path = self.file_path_to_url(file_path, public_dir)?;
        Ok((url_path, route))
    }

    // #[inline(always)] forces the compiler to inline this function at call sites
    // Used for hot path functions where call overhead matters more than code size
    #[inline(always)]
    pub fn get_route(&self, path: &str) -> Option<CachedRoute> {
        // CLONE FOR LOCK RELEASE: DashMap::get returns a lock guard. We must clone to avoid
        // holding the concurrent lock while serving the response. Since CachedRoute contains
        // only Arcs, this clone just increments reference counters (cheap) rather than
        // copying actual data. Alternative would be holding the lock during entire HTTP response.
        self.routes.get(path).map(|entry| entry.value().clone())
    }

    fn file_path_to_url(&self, file_path: &Path, public_dir: &Path) -> Result<Arc<str>> {
        let relative = file_path.strip_prefix(public_dir)?;
        let url_path = format!("/{}", relative.to_string_lossy().replace('\\', "/"));
        Ok(Arc::from(url_path.as_str()))
    }

    fn generate_etag(&self, modified: &SystemTime, content: &[u8]) -> String {
        use std::time::UNIX_EPOCH;

        let timestamp = modified
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();

        // Simple ETag: timestamp-size
        format!("\"{:x}-{:x}\"", timestamp, content.len())
    }

    fn format_http_date(&self, time: SystemTime) -> String {
        let datetime = chrono::DateTime::<chrono::Utc>::from(time);
        datetime.format("%a, %d %b %Y %H:%M:%S GMT").to_string()
    }
}

// Lock-free atomic operations for route updates in dev mode
impl NanoWeb {
    pub fn refresh_if_modified(
        &self,
        path: &str,
        config_prefix: &str,
    ) -> Result<Option<CachedRoute>> {
        if let Some(route_ref) = self.routes.get(path) {
            let route = route_ref.value().clone();
            drop(route_ref); // Release the reference early

            // &*route.path - Arc<PathBuf> -> PathBuf -> &PathBuf -> &Path
            // std::fs::metadata needs AsRef<Path>, so we dereference Arc then take reference
            // Alternative: route.path.as_ref() but &* is more idiomatic in Rust
            let metadata = std::fs::metadata(&*route.path)?;
            let modified = metadata.modified()?;

            if modified > route.modified {
                debug!("File modified, refreshing route: {:?}", route.path);

                // Create new route
                let parent_dir = route.path.parent().unwrap();
                let public_dir = parent_dir.ancestors().last().unwrap();
                let (_, new_route) =
                    self.create_route(&route.path, &metadata, public_dir, config_prefix)?;

                // Atomic update
                self.routes.insert(Arc::from(path), new_route.clone());
                return Ok(Some(new_route));
            }
            Ok(Some(route))
        } else {
            Ok(None)
        }
    }
}
