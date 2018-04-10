FROM microsoft/dotnet:2.0-sdk-jessie
WORKDIR /aspnetcore
COPY Benchmarks Benchmarks
COPY run-linux.sh run-linux.sh
COPY setup-mvc.sh setup-mvc.sh
CMD bash setup-mvc-plaintext.sh
