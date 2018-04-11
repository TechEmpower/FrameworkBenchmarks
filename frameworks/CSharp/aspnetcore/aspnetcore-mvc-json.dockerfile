FROM microsoft/dotnet:2.0-sdk-jessie
WORKDIR /aspnetcore
COPY Benchmarks Benchmarks
COPY run-linux.sh run-linux.sh
COPY setup-mvc-json.sh setup-mvc-json.sh
CMD bash setup-mvc-json.sh
