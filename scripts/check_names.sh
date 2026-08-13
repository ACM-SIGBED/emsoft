#!/bin/bash

# 2026-08-13 T.Bourke with Gemini

# Trap Ctrl-C (SIGINT) and exit the entire script instantly
trap "echo -e '\nScript aborted by user.'; exit 1" INT

# Function to manually flatten accents to standard text using sed
strip_accents() {
    echo "$1" | sed '
        s/[ÁÀÂÄÃÅ]/A/g; s/[áàâäãå]/a/g;
        s/[ÉÈÊË]/E/g;   s/[éèêë]/e/g;
        s/[ÍÌÎÏ]/I/g;   s/[íìîï]/i/g;
        s/[ÓÒÔÖÕØ]/O/g; s/[óòôöõø]/o/g;
        s/[ÚÙÛÜ]/U/g;   s/[úùûü]/u/g;
        s/[Ñ]/N/g;       s/[ñ]/n/g;
        s/[Ç]/C/g;       s/[ç]/c/g;
        s/[ÝŸ]/Y/g;      s/[ýÿ]/y/g;
        s/[Æ]/AE/g;     s/[æ]/ae/g;
        s/[Œ]/OE/g;     s/[œ]/oe/g;
        s/[ß]/ss/g;
    '
}

# Check if a filename was provided as an argument
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <filename>"
    exit 1
fi

input_file="$1"

# Check if the file exists
if [ ! -f "$input_file" ]; then
    echo "Error: File '$input_file' not found."
    exit 1
fi

# Read the file line by line
while IFS= read -r line || [[ -n "$line" ]]; do
    # Skip empty lines
    [[ -z "${line// }" ]] && continue

    # Parse out the LASTNAME and FIRSTNAME.
    # This expects: "LASTNAME, FIRSTNAME [OTHER NAMES]"
    # It splits by the comma, removes leading/trailing spaces, and isolates the first name.
    if [[ "$line" =~ ^([^,]+),\ *([^ ]+) ]]; then
        lastname="${BASH_REMATCH[1]}"
        firstname="${BASH_REMATCH[2]}"

	clean_first=$(strip_accents "$firstname")
	clean_last=$(strip_accents "$lastname")
        
        # Get the first letter of the last name and convert it to lowercase
        first_letter="${lastname:0:1}"
	subdir=$(echo "$first_letter" | tr '[:upper:]' '[:lower:]')

        # Print the heading
        echo "========================================"
        echo "NAME: $line"
        echo "========================================"

        # Command 1: Find by author using case-insensitive regex
        echo "--> Finding by author (contains '$clean_last'):"
	find by_name -type f 2>/dev/null | while read -r filepath; do
            flat_path=$(strip_accents "$filepath")
            if echo "$flat_path" | grep -iq "$clean_last"; then
                echo "$filepath"
            fi
        done
        
        echo "" # Newline for readability

        # Command 2: List subdirectory and grep for first name
        echo "--> Listing by_name/${subdir} (matches '$clean_first'):"
        if [ -d "by_name/${subdir}" ]; then
	    ls "by_name/${subdir}" | while read -r filename_item; do
                flat_file=$(strip_accents "$filename_item")
                if echo "$flat_file" | grep -iq "$clean_first"; then
                    echo "$filename_item"
                fi
	    done
        else
            echo "Directory 'by_name/${subdir}' does not exist."
        fi
        
        echo "" # Extra spacing between names
    else
        echo "Skipping malformed line: $line"
        echo ""
    fi
done < "$input_file"

