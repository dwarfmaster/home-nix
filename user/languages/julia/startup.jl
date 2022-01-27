
try
    using Revise
    ENV["JULIA_REVISE"] = "auto"
catch e
    @warn "Error initializing Revise" exception=(e, catch_backtrace())
end
