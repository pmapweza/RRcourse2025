# Render to themed HTML with table of contents
pandoc suits_report.md -o suits_report_themed_toc.html --toc --css=github-v2.css

# Render to plain text
pandoc suits_report.md -o suits_report.txt