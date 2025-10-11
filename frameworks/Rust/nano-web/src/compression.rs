use anyhow::Result;
use bytes::Bytes;
use flate2::write::GzEncoder;
use flate2::Compression;
use std::io::Write;

#[derive(Debug, Clone)]
pub struct CompressedContent {
    pub plain: Bytes,
    pub gzip: Option<Bytes>,
    pub brotli: Option<Bytes>,
    pub zstd: Option<Bytes>,
}

impl CompressedContent {
    pub fn new(content: Vec<u8>, should_compress: bool) -> Result<Self> {
        let plain = Bytes::from(content);

        if !should_compress || plain.len() < 1024 {
            return Ok(Self {
                plain,
                gzip: None,
                brotli: None,
                zstd: None,
            });
        }

        let plain_ref = &plain;
        let (gzip, (brotli, zstd)) = rayon::join(
            || gzip_compress(plain_ref),
            || rayon::join(|| brotli_compress(plain_ref), || zstd_compress(plain_ref)),
        );

        Ok(Self {
            plain,
            gzip: Some(gzip?),
            brotli: Some(brotli?),
            zstd: Some(zstd?),
        })
    }

    pub fn get_best_encoding(&self, accept_encoding: &str) -> (&'static str, &Bytes) {
        if accept_encoding.contains("zstd") && self.zstd.is_some() {
            ("zstd", self.zstd.as_ref().unwrap())
        } else if accept_encoding.contains("br") && self.brotli.is_some() {
            ("br", self.brotli.as_ref().unwrap())
        } else if accept_encoding.contains("gzip") && self.gzip.is_some() {
            ("gzip", self.gzip.as_ref().unwrap())
        } else {
            ("identity", &self.plain)
        }
    }
}

pub fn gzip_compress(data: &[u8]) -> Result<Bytes> {
    let mut encoder = GzEncoder::new(Vec::new(), Compression::best());
    encoder.write_all(data)?;
    let compressed = encoder.finish()?;
    Ok(Bytes::from(compressed))
}

pub fn brotli_compress(data: &[u8]) -> Result<Bytes> {
    let mut compressed = Vec::new();
    let mut cursor = std::io::Cursor::new(data);
    let params = brotli::enc::BrotliEncoderParams {
        quality: 11, // Maximum quality (0-11)
        ..Default::default()
    };
    brotli::BrotliCompress(&mut cursor, &mut compressed, &params)?;
    Ok(Bytes::from(compressed))
}

pub fn zstd_compress(data: &[u8]) -> Result<Bytes> {
    let compressed = zstd::bulk::compress(data, 22)?; // Maximum level (1-22)
    Ok(Bytes::from(compressed))
}
