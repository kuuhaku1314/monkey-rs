$ErrorActionPreference = "Stop"

$Root = Resolve-Path (Join-Path $PSScriptRoot "..")
Set-Location $Root

$metadata = cargo metadata --no-deps --format-version 1 | ConvertFrom-Json
$version = $metadata.packages[0].version
$dist = Join-Path $Root "dist"
$targetName = "monkey-rs-$version-windows-$env:PROCESSOR_ARCHITECTURE.exe"

New-Item -ItemType Directory -Force -Path $dist | Out-Null
cargo build --release
Copy-Item (Join-Path $Root "target\release\monkey-rs.exe") (Join-Path $dist $targetName) -Force
Write-Output (Join-Path $dist $targetName)
