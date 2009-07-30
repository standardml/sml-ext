
structure StandardGlpkLp = StandardLpFn(GlpkLpBase)
structure GlpkLp : LP = LpFn(StandardGlpkLp)

