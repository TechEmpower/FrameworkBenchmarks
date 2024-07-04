import happyx

serve "127.0.0.1", 5000:
  get "/json":
    return {"message": "Hello, World!"}

  get "/plaintext":
    return "Hello, World!"
