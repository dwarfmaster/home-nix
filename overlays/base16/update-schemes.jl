#!/usr/bin/env julia

using HTTP
using YAML
using LibGit2

function GetFileExtension(filename)
    index=findlast(isequal('.'),filename)
    if isnothing(index)
        return filename, ""
    elseif index == 1
        return "", filename
    else
        return filename[begin:index-1], filename[index:end]
    end
end

function ProcessScheme(tempdir, nix, scheme_name, scheme_repo)
    println("$scheme_name => $scheme_repo :")
    write(nix, "  $scheme_name = {\n")

    dir = Base.Filesystem.joinpath(tempdir, scheme_name)
    LibGit2.clone(scheme_repo, dir)
    names = Dict{String,Int}()
    for yaml_file in Base.Filesystem.readdir(dir; join=false, sort=false)
        base, ext = GetFileExtension(yaml_file)
        if !(ext == ".yaml" || ext == ".yml")
            continue
        end
        scheme = YAML.load_file(Base.Filesystem.joinpath(dir, yaml_file))
        name = scheme["scheme"]
        if haskey(names, name)
            count = names[name]
            names[name] = names[name] + 1
            name = "$name#$count"
        else
            names[name] = 1
        end
        println("  - $name : $yaml_file")

        write(nix, "    \"$name\" = {\n");
        for color in [ "base00" "base01" "base02" "base03" "base04" "base05" "base06" "base07"
                        "base08" "base09" "base0A" "base0B" "base0C" "base0D" "base0E" "base0F" ]
            r = scheme[color][1:2]
            g = scheme[color][3:4]
            b = scheme[color][4:5]
            write(nix, "      $color.hex = { r = \"$r\"; g = \"$g\"; b = \"$b\"; };\n")
        end
        write(nix, "    };\n")
    end

    Base.Filesystem.rm(dir; force=true, recursive=true)
    write(nix, "  };\n");
end

schemes = Dict{String,Dict{String,String}}()

open("schemes.nix", "w") do nix
    write(nix, "{\n");
    tempdir = Base.Filesystem.mktempdir(; prefix="nix_base16_", cleanup=true)
    print("Storing temporary files in ")
    println(tempdir)

    schemes_url = "https://raw.githubusercontent.com/chriskempson/base16-schemes-source/master/list.yaml"
    schemes_yaml = String(HTTP.request("GET", schemes_url).body)

    for (scheme_name,scheme_repo) in YAML.load(schemes_yaml)
        ProcessScheme(tempdir, nix, scheme_name, scheme_repo)
    end
    write(nix, "}\n");
end
