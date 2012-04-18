guard :shell do
  watch(%r{^src/.+\.erl}) {`rebar doc`}
  watch("README.md") {`markdown --html4docs README.md > README.html`}
end
