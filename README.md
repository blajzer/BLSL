# BLSL

BLSL is a modern, high-level shading language that acts as a frontend to the various API/platform-level shading languages.

The motivating forces behind writing an entirely new language rather than improving tooling for an existing shader language are thus:
1. **Stronger typing** - Existing shader languages have fairly weak typing and in the case of HLSL will even do vector narrowing/promotion in very ad-hoc ways. This can introduce (and definitely has introduced) errors that are difficult to diagnose.
1. **Robust Variants/Permutations** - Shader variants or shader permutations are a common and necessary evil for handling a shader that has features that can be toggled on or off. BLSL will support not just boolean variants, but also integer variants as a top-level construct.
1. **No #ifdefs** - An additional aspect of shader variants is that they are often implemented using the C Preprocessor. Unfortunately, the preprocessor runs at a very early stage of compilation and is destructive &mdash; any code removed by the preprocessor is gone before any other analysis or parsing can occur. In the case of BLSL, variants are first-class citizens and are a part of the source itself, rather than being encoded in a meta-language, so we can do analysis and retain information that would otherwise be lost.
1. **Better Syntax/Semantics** - Not having to be backward compatible (with anything) means that BLSL can have syntax more closely related to the direction that programmable graphics has gone in, and also implement things like first-class functions or associated functions/extension methods that can drastically improve ergonomics. There's no concrete plan for generics at the moment, but they're definitely a possible feature.

This project is still in its _extremely early stages_, so I don't recommend trying to use it at this point, although anyone interested in contributing should contact me.

## License
BLSL is licensed under the terms of the Mozilla Public License v2.0. See https://mozilla.org/MPL/2.0/ for the full text of the license.

## Feature Roadmap
- [ ] Formal Syntax
- [ ] Parser
- [ ] Validation and Type Checking
- [ ] Module System
- [ ] Specialization/Monomorphization (maybe, still considering generics)
- [ ] SSA Conversion Pass
- [ ] Dead Code Eliminator
- [ ] Inlining
- [ ] Output Modules (SPIR-V/HLSL/GLSL/etc...)
