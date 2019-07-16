import subprocess


def get_x_resources():
    "Returns a dictionary of X resource keys to values."
    xrdb_output = subprocess.run(
        ["xrdb", "-query"], check=True, stdout=subprocess.PIPE
    ).stdout.decode("ascii")
    x_resources = {}

    for line in xrdb_output.splitlines():
        [key, value] = line.split(":")
        x_resources[key.strip()] = value.strip()

    return x_resources


def print_kitty_theme(x_resources):
    "Prints out the theme configuration for kitty based on X_RESOURCES."
    print(
        """# automatically generated from Xresources

# cursor
cursor            {0}
cursor_text_color background

# colors
foreground           {1}
background           {2}
selection_foreground {2}
selection_background {1}
# black
color0  {3}
color8  {4}
# red
color1  {5}
color9  {6}
# green
color2  {7}
color10 {8}
# yellow
color3  {9}
color11 {10}
# blue
color4  {11}
color12 {12}
# magenta
color5  {13}
color13 {14}
# cyan
color6  {15}
color14 {16}
# white
color7  {17}
color15 {18}""".format(
            x_resources["*cursorColor"],
            x_resources["*foreground"],
            x_resources["*background"],
            x_resources["*color0"],
            x_resources["*color8"],
            x_resources["*color1"],
            x_resources["*color9"],
            x_resources["*color2"],
            x_resources["*color10"],
            x_resources["*color3"],
            x_resources["*color11"],
            x_resources["*color4"],
            x_resources["*color12"],
            x_resources["*color5"],
            x_resources["*color13"],
            x_resources["*color6"],
            x_resources["*color14"],
            x_resources["*color7"],
            x_resources["*color15"],
        )
    )


def main():
    print_kitty_theme(get_x_resources())


if __name__ == "__main__":
    main()
