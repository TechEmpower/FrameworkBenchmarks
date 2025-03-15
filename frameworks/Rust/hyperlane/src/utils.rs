use crate::*;

#[inline]
pub fn generate_rfc1123_timestamp() -> String {
    let now: DateTime<Utc> = SystemTime::now().into();
    now.format("%a, %d %b %Y %H:%M:%S GMT").to_string()
}

#[inline]
pub fn raw_helper(
    h: &Helper,
    _: &Handlebars,
    _: &Context,
    _: &mut RenderContext,
    out: &mut dyn handlebars::Output,
) -> HelperResult {
    if let Some(param) = h.param(0) {
        let value = param.value().as_str().unwrap_or_default();
        out.write(value)?;
    }
    Ok(())
}
