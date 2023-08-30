# EMSOFT: International Conference on Embedded Software

This repository contains metadata for the EMSOFT conference.

Feel free to correct the content and especially to provide links to the 
author's versions of the articles (access must not require registration or 
payment), either by submitting a pull request or sending an 
[email](mailto:timothy.bourke@inria.fr).

**EMSOFT Chairs**: To add a new conference entry, simply copy the previous 
entries in `articles` and `pc` and edit them in the obvious way. Be careful 
not to introduce unwanted line breaks. Type `make validate` to check that 
the script can parse the new file. Type `make summaries` to update the name 
index. Submit a pull request or send a mail to update the repository. If you 
like, you could encourage authors to provide unencumbered links to their 
articles.

* EMSOFT 2023: [articles](articles/emsoft2023.md), [pc](pc/emsoft2023-pc.md)
* EMSOFT 2022: [articles](articles/emsoft2022.md), [pc](pc/emsoft2022-pc.md)
* EMSOFT 2021: [articles](articles/emsoft2021.md), [pc](pc/emsoft2021-pc.md)
* EMSOFT 2020: [articles](articles/emsoft2020.md), [pc](pc/emsoft2020-pc.md)
* EMSOFT 2019: [articles](articles/emsoft2019.md), [pc](pc/emsoft2019-pc.md)
* EMSOFT 2018: [articles](articles/emsoft2018.md), [pc](pc/emsoft2018-pc.md)
* EMSOFT 2017: [articles](articles/emsoft2017.md), [pc](pc/emsoft2017-pc.md)
* EMSOFT 2016: [articles](articles/emsoft2016.md), [pc](pc/emsoft2016-pc.md)
* EMSOFT 2015: [articles](articles/emsoft2015.md), [pc](pc/emsoft2015-pc.md)
* EMSOFT 2014: [articles](articles/emsoft2014.md), [pc](pc/emsoft2014-pc.md)
* EMSOFT 2013: [articles](articles/emsoft2013.md), [pc](pc/emsoft2013-pc.md)
* EMSOFT 2012: [articles](articles/emsoft2012.md), [pc](pc/emsoft2012-pc.md)
* EMSOFT 2011: [articles](articles/emsoft2011.md), [pc](pc/emsoft2011-pc.md)
* EMSOFT 2010: [articles](articles/emsoft2010.md), [pc](pc/emsoft2010-pc.md)
* EMSOFT 2009: [articles](articles/emsoft2009.md), [pc](pc/emsoft2009-pc.md)
* EMSOFT 2008: [articles](articles/emsoft2008.md), [pc](pc/emsoft2008-pc.md)
* EMSOFT 2007: [articles](articles/emsoft2007.md), [pc](pc/emsoft2007-pc.md)
* EMSOFT 2006: [articles](articles/emsoft2006.md), [pc](pc/emsoft2006-pc.md)
* EMSOFT 2005: [articles](articles/emsoft2005.md), [pc](pc/emsoft2005-pc.md)
* EMSOFT 2004: [articles](articles/emsoft2004.md), [pc](pc/emsoft2004-pc.md)
* EMSOFT 2003: [articles](articles/emsoft2003.md), [pc](pc/emsoft2003-pc.md)
* EMSOFT 2002: [articles](articles/emsoft2002.md), [pc](pc/emsoft2002-pc.md)
* EMSOFT 2001: [articles](articles/emsoft2001.md)

## Format

The files are formatted in markdown with the idea that they could also be 
manipulated by scripts. All names are formatted as `last, first`. Lists of 
names are separated by vertical bars (`|`).

### Articles files
1. A header (`#`) naming the conference. The conference name must begin with 
   the sequence: acronym, space, year, colon, full title. For example, 
   `EMSOFT 2022: 22nd International Conference on Embedded Software`.
2. A list of program chair names.
3. For each session:
    1. A header (`##`) with the session title
    2. A list of articles
3. For each article:
    1. A header (`###`) with the article title
    2. A list of author names
    3. *optional*: `* DOI: [doi](https://doi.org/doi)`
    4. *optional*: `* [URL](link-to-freely-available-author-version)`

### Program committee files
1. A header (`#`) naming the conference.
2. One line per program committee member (beginning with a `*`).
   Chairs first and otherwise in alphabetical order.

### Names files

The reverse mapping in `by_names` from author names to articles is generated 
automatically (`make summaries`).

## Scripts

The [scripts/procproc.ml](scripts/procproc.ml) script can validate (`make`) 
or rewrite (`make rewrite`) the article files.

It should be easy to adapt it to display conference informations (e.g., 
`make authors`) or to output the conference information in a different 
format (e.g., in html for a website or LaTeX for a report).

The script is written in [OCaml >= 4.11.1](https://ocaml.org). The easiest 
way to install the necessary dependencies is to run
`sudo apt-get -y install ocaml` (or similar).

