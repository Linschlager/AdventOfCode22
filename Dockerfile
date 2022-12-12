FROM mcr.microsoft.com/dotnet/runtime:6.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY ["AdventOfCode.fsproj", "./"]
RUN dotnet restore "AdventOfCode.fsproj"
COPY . .
WORKDIR "/src/"
RUN dotnet build "AdventOfCode.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "AdventOfCode.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "AdventOfCode.dll"]
