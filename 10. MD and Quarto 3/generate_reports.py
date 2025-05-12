import quarto
import os

for season_number in range(1, 9):
    output_file = f"season{season_number}_report.pdf"
    quarto.render(
        "Assignment.qmd",
        output_file=output_file,
        params={"season": season_number}
    )
    print(f"Generated report for Season {season_number} at {os.path.abspath(output_file)}")

print("Finished generating reports for all seasons.")