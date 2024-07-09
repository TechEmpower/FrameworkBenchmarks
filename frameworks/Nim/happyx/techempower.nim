import happyx

serve "0.0.0.0", 5000:
  get "/json":
    return {"message": "Hello, World!"}

  get "/plaintext":
    return "Hello, World!"
