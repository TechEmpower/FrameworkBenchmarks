FROM microsoft/dotnet:2.0-sdk-jessie
WORKDIR /aspnetcore
COPY Benchmarks Benchmarks
COPY run-linux.sh run-linux.sh
COPY setup-mvc-dapper.sh setup-mvc-dapper.sh
CMD bash setup-mvc-dapper.sh
