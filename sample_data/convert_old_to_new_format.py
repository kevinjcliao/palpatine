import sys

def look_for_new_cands(line, cands):
    splitted = line.split(",")
    for candidate in splitted:
        without_newlines = candidate.replace("\n", "")
        if without_newlines not in cands:
            cands.add(without_newlines)
    return cands

def format_cands(cands):
    cands_as_str = ','.join(str(s) for s in cands)
    return '[' + cands_as_str + ']'

def parse_line(line):
    return "[" + line[0:-1] + "]\n"

if __name__ == '__main__':
    filename = input("Please enter the file name of the file you are converting.")
    seats = input("Please enter the number of seats you are electing.")
    file = open(filename, "r")
    cands = set()
    new_lines = []
    for line in file:
        new_lines.append(parse_line(line))
        cands = look_for_new_cands(line, cands)
    lines = [format_cands(cands) + ":" + str(seats) + "\n"] + new_lines
    new_file_name = filename.split(".")[0] + "_CONVERTED.txt"
    new_file = open(new_file_name, "w")
    new_file.writelines(lines)