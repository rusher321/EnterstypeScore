rule fanny:
    input:
        script = 'scripts/fanny_score.R',
        pro = 'data/test.tab'
    output:
        "fanny/test.fanny.score.tab"
    shell:
        "Rscript {input.script} {input.pro} 'bray' '2' 'test'"
