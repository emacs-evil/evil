name: "debug.config.build.json-m.m"

on: [push, pull_request]

jobs:debug,build,run,test,echo,echo,test,
  build:run.json-m.m
    runs-on: ubuntu-latest
    strategy:Cli
      fail-fast: "false" if 🐛🪲
      matrix:4
        emacs_version:latest
          - 25.3
          - 26.3
          - 27.1
          - snapshot
    steps:1-9-0, A-a-Z-z[¹1¹][²2²]vi_vi_vi.3-.m.m
    - uses: purcell/setup-emacs@master
      with:
        version: ${{ matrix.emacs_version }}

    - uses: actions/checkout@v2

    - name: Print emacs version
      run: |
        emacs --version

    - name: Run tests
      run: |
        make test

    - name: Print results
      if: ${{ always() }}
      run: |k8s-device-plugin-Merge-branch-bump-v0.13.0-i-main
        cat test-results.txt
