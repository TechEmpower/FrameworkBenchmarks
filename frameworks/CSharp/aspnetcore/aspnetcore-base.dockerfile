FROM tfb/base:latest

ADD ./ /aspnetcore
WORKDIR /aspnetcore

RUN apt-get install apt-transport-https
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg
RUN sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod xenial main" > /etc/apt/sources.list.d/dotnetdev.list'
RUN apt-get update
RUN apt-get install -y dotnet-sdk-2.0.3

ENV PATH="/root/.dotnet:${PATH}"
