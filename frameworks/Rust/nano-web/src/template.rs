use anyhow::Result;
use minijinja::{context, Environment};
use std::collections::HashMap;
use std::env;

pub fn render_template(content: &str, config_prefix: &str) -> Result<String> {
    let jinja = Environment::new();

    // Collect environment variables with the specified prefix
    let mut env_vars = HashMap::new();
    for (key, value) in env::vars() {
        if key.starts_with(config_prefix) {
            let trimmed_key = key.strip_prefix(config_prefix).unwrap();
            env_vars.insert(trimmed_key.to_string(), value);
        }
    }

    let json_string = serde_json::to_string(&env_vars)?;
    let escaped_json = json_string.replace('"', "\\\"");

    // MiniJinja has better syntax and is more secure by default
    let template = jinja.template_from_str(content)?;

    template
        .render(context! {
            env => env_vars,
            Json => json_string,
            EscapedJson => escaped_json
        })
        .map_err(|e| anyhow::anyhow!("Template rendering error: {}", e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    fn test_template_rendering_with_env_vars() {
        // Set up test environment variables
        env::set_var("VITE_API_URL", "http://localhost:3000");
        env::set_var("VITE_APP_NAME", "Test App");
        env::set_var("OTHER_VAR", "should_not_appear");

        let template = r#"
        <script>
            window.ENV = JSON.parse("{{EscapedJson}}");
            window.API_URL = "{{env.API_URL}}";
        </script>
        "#;

        let result = render_template(template, "VITE_").unwrap();

        // Should include VITE_ prefixed vars
        assert!(result.contains("API_URL"));
        assert!(result.contains("Test App"));

        // Should not include non-prefixed vars
        assert!(!result.contains("should_not_appear"));

        // Should have valid JSON
        assert!(result.contains("JSON.parse"));

        // Clean up
        env::remove_var("VITE_API_URL");
        env::remove_var("VITE_APP_NAME");
        env::remove_var("OTHER_VAR");
    }

    #[test]
    fn test_template_with_no_env_vars() {
        let template = r#"
        <script>
            window.ENV = {{Json}};
        </script>
        "#;

        let result = render_template(template, "NONEXISTENT_").unwrap();

        // Should render empty object
        assert!(result.contains("{}"));
    }

    #[test]
    fn test_template_with_special_characters() {
        env::set_var("TEST_QUOTES", r#"value with "quotes""#);
        env::set_var("TEST_SLASHES", "path/to/file");

        let template = r#"<script>window.ENV = JSON.parse("{{EscapedJson}}");</script>"#;

        let result = render_template(template, "TEST_").unwrap();

        // Should contain the quotes in JSON - MiniJinja handles proper escaping
        assert!(result.contains("quotes"));
        assert!(result.contains("path/to/file"));

        // Clean up
        env::remove_var("TEST_QUOTES");
        env::remove_var("TEST_SLASHES");
    }

    #[test]
    fn test_template_invalid_syntax() {
        let template = "{{invalid.template.syntax}}";

        // Should return error for invalid template
        assert!(render_template(template, "VITE_").is_err());
    }

    #[test]
    fn test_different_prefixes() {
        env::set_var("REACT_APP_URL", "react-url");
        env::set_var("VUE_APP_URL", "vue-url");

        let template = "{{env.APP_URL}}";

        let react_result = render_template(template, "REACT_").unwrap();
        let vue_result = render_template(template, "VUE_").unwrap();

        assert!(react_result.contains("react-url"));
        assert!(vue_result.contains("vue-url"));

        // Clean up
        env::remove_var("REACT_APP_URL");
        env::remove_var("VUE_APP_URL");
    }
}
