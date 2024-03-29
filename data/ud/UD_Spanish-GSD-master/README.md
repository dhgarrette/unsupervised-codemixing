# Summary

The Spanish UD is converted from the content head version of the [universal
dependency treebank v2.0 (legacy)](https://github.com/ryanmcd/uni-dep-tb).


# Introduction

In addition to converting dependencies from the legacy UD treebank, token level morphology features have been added
automatically using the parsers/taggers in Bohnet et al 2014* and Bohnet et al. 2015** trained on the Ancora***
treebank and converted automatically to UD standards.

Various heuristics have been added to improve the output of the tagger, fix obvious errors and add features that
the tagger did not supply. The changes for v1.2 (November 2015) were done by Miguel Ballesteros, Dan Zeman, and
Héctor Martínez Alonso.

The Spanish UD conforms to the UD guidelines, but there are some exceptions.

* Bohnet, Bernd, et al. "Joint morphological and syntactic analysis for richly inflected languages."
  Transactions of the Association for Computational Linguistics 1 (2013): 415-428.
* Bohnet, Bernd, et al. "Static and Dynamic Feature Selection in Morphosyntactic Analyzers." Under review. 2015.
* Taulé, Mariona, Maria Antònia Martí, and Marta Recasens.
  "AnCora: Multilevel Annotated Corpora for Catalan and Spanish." LREC. 2008.


# Changelog

* 2017-03-01 v2.0
  * Converted to UD v2 guidelines.
* 2015-11-15 v1.2
  * Removed duplicite sentences (train overlapped with dev/test, removed from dev/test).
  * Ensured AUX are not heads.
  * Applied type-based POS changes for function words.
  * Changed mwe and name relations to left-headed.
  * Added morphology features (tagger + heuristic rules and semi-automatic fixes).
  * Added lemmas (lemmatizer + heuristic rules).
* 2015-05-15 v1.1
  * Second release.
* 2015-01-15 v1.0
  * First release. No lemmas, no features. Dependencies converted from the legacy UD treebank.



###############################################################################
LEGACY README FILE BELOW
###############################################################################

===================================
Universal Dependency Treebanks v2.0
===================================

This directory contains treebanks for the following languages:
  English, German, French, Spanish, Swedish, Korean, Japanese, Indonesian,
  Brazilian Portuguese, Italian and Finnish.

A description of how the treebanks were generated can be found in:

  Universal Dependency Annotation for Multilingual Parsing
  Ryan McDonald, Joakim Nivre, Yvonne Quirmbach-Brundage, Yoav Goldberg,
  Dipanjan Das, Kuzman Ganchev, Keith Hall, Slav Petrov, Hao Zhang,
  Oscar Tackstrom, Claudia Bedini, Nuria Bertomeu Castello and Jungmee Lee
  Proceedings of ACL 2013

A more detailed description of each relation type in our harmonized scheme is
included in the file universal-guidelines.pdf.

Each treebank has been split into training, development and testing files.

Each file is formatted according to the CoNLL 2006/2007 guidelines:

  http://ilk.uvt.nl/conll/#dataformat

The treebank annotations use basic Stanford Style dependencies, modified
minimally to be sufficient for each language and be maximally consistent across
languages. The original English Stanford guidelines can be found here:

  http://nlp.stanford.edu/software/dependencies_manual.pdf


==============================================================================
Version 2.0 - What is new
==============================================================================

1. Added more data for German, Spanish and French.
2. Added Portuguese Brazilian, Indonesian, Japanese, Italian and Finnish.
3. New content-head versions for 5 languages (see below).
4. A number of bug fixes in the harmonization process.

=====================
Standard dependencies
=====================

In release 2.0 we include two sets of dependencies. The first is standard
Stanford dependencies, which correspond roughly to the output of the
Stanford converter for English with the copula as head set to true. In
general, these are content-head dependency representations with two major
exceptions: 1) adpositions are the head in adpositional phrases, and 2) copular
verbs are the head in copluar constructions.

This data is in the std/ directory and contains all languages but Finnish.

Version 1.0 of the data is only standard.

==========================
Content head dependencies
==========================

In order to converge to a more uniform multilingual standard, in particular
for morphologically rich languages, this release also includes a beta version
of content-head dependencies for five languages: German, Spanish, Finnish,
French and Swedish. Here the content word is always the head of a phrase.

=============================================================================
Language Specific Information
=============================================================================

====================
English dependencies
====================

Note that the English dependencies are based on the original Penn Treebank data
automatically converted with the Stanford Dependency Converter. Instructions for
how to do this with corresponding scripts are included in the English directory.

====================
Finnish dependencies
====================

Finnish data is in the ch/fi directory and was produced by researchers at
the University of Turku. In that directory there are specific README and
LICENSE files for that data. Two things to note. First, the Finnish data is
only content-head. This is due to difficulties in automatically converting the
data to standard format from its original annotations. Second, we have included
a test set in the release, but this is not the real test set, just a subset of
the training. The true test set for this data is blind (as per the wishes of
the researchers at Turku). The unannotated test data is included as well as
instructions for obtaining scores on predictions.

=============================================================================
Other Information
=============================================================================

================================
Fine-grained part-of-speech tags
================================

In the CoNLL file format there is a coarse part-of-speech tag field (4) and a
fine-grained part-of-speech tag field (5). In this data release, we use the
coarse field to store the normalized universal part-of-speech tags that are
consistent across languages. The fine-grained field contains potentially richer
part-of-speech information depending on the language, e.g., a richer tag
representation for clitics.

=========================
Licenses and terms-of-use
=========================

For the following languages

  German, Spanish, French, Indonesian, Italian, Japanese, Korean and Brazilian
  Portuguese

we will distinguish between two portions of the data.

1. The underlying text for sentences that were annotated. This data Google
   asserts no ownership over and no copyright over. Some or all of these
   sentences may be copyrighted in some jurisdictions.  Where copyrighted,
   Google collected these sentences under exceptions to copyright or implied
   license rights.  GOOGLE MAKES THEM AVAILABLE TO YOU 'AS IS', WITHOUT ANY
   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED.

2. The annotations -- part-of-speech tags and dependency annotations. These are
   made available under a CC BY-NC-SA 3.0 non commercial license. GOOGLE MAKES
   THEM AVAILABLE TO YOU 'AS IS', WITHOUT ANY WARRANTY OF ANY KIND, WHETHER
   EXPRESS OR IMPLIED. See attached LICENSE file for the text of CC BY-NC-SA.

Portions of the German data were sampled from the CoNLL 2006 Tiger Treebank
data. Hans Uszkoreit graciously gave permission to use the underlying
sentences in this data as part of this release.

For English, Italian, Finnish and Swedish, please see licences included in
these directories or the following sources.

Finnish - http://bionlp.utu.fi/fintreebank.html
Swedish - http://stp.lingfil.uu.se/~nivre/swedish_treebank/
Italian - http://medialab.di.unipi.it/wiki/ISDT

We are greatful to researchers at those institutes who provided us data, in
particular:

Maria Simi and company from the University of Pisa.
  Converting Italian Treebanks: Towards an Italian Stanford Dependency Treebank
  Bosco, Cristina and Montemagni, Simonetta and Simi, Maria
  Proceedings of LAW VII \& ID

Filip Ginter and company from the University of Turku.
  Building the essential resources for Finnish: the Turku Dependency Treebank
  Haverinen, Katri and Nyblom, Jenna and Viljanen, Timo and Laippala,
  Veronika and Kohonen, Samuel and Missil{\"a}, Anna and Ojala, Stina and
  Salakoski, Tapio and Ginter, Filip
  Language Resources and Evaluation, 2013

Joakim Nivre and company from Uppsala University.

And Chris Manning and Marie-Catherine de Marneffe from Stanford and Ohio.
  Generating typed dependency parses from phrase structure parses
  MC De Marneffe, B MacCartney, CD Manning,
  Proceedings of LREC, 2006

Any use of the data should reference the above plus:

  Universal Dependency Annotation for Multilingual Parsing
  Ryan McDonald, Joakim Nivre, Yvonne Quirmbach-Brundage, Yoav Goldberg,
  Dipanjan Das, Kuzman Ganchev, Keith Hall, Slav Petrov, Hao Zhang,
  Oscar Tackstrom, Claudia Bedini, Nuria Bertomeu Castello and Jungmee Lee
  Proceedings of ACL 2013

=======
Contact
=======

ryanmcd@google.com
joakim.nivre@lingfil.uu.se
slav@google.com


=== Machine-readable metadata =================================================
Data available since: UD v1.0
License: CC BY-NC-SA 3.0 US
Includes text: yes
Genre: blog news reviews wiki
Lemmas: automatic
UPOS: converted from manual
XPOS: not available
Features: automatic
Relations: converted from manual
Contributors: Ballesteros, Miguel; Martínez Alonso, Héctor; McDonald, Ryan; Pascual, Elena; Silveira, Natalia; Zeman, Daniel; Nivre, Joakim
Contributing: here
Contact: zeman@ufal.mff.cuni.cz
Paragraphs to web: 2
===============================================================================
(Original treebank contributors: Quirmbach-Brundage, Yvonne; LaMontagne, Adam; Souček, Milan; Järvinen, Timo; Radici, Alessandra)
