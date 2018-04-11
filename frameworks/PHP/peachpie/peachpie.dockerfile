FROM microsoft/dotnet:2.0-sdk-jessie

ADD ./ /peachpie
WORKDIR /peachpie

ENV DOTNET_SKIP_FIRST_TIME_EXPERIENCE true
ENV DOTNET_CLI_TELEMETRY_OPTOUT true

ENV PATH="/root/.dotnet:${PATH}"

CMD dotnet run -p Server -c Release
