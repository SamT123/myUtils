get_consensus = function(seq_chars) {
  map_chr(
    1:str_length(seq_chars[[1]]),
    function(pos, seq_chars) {
      t = substr(seq_chars, pos, pos) %>% table()
      t = t[names(t) != 'X']
      aa = t[which.max(t)]
      return(aa)
    },
    seq_chars = seq_chars
  ) -> x
  paste(x, collapse = '')
}


get_consensus_biostring = function(sequence_biostrings) {
  sequence_biostrings = str_replace_all(
    sequence_biostrings,
    fixed('*'),
    fixed('X')
  )

  x = Biostrings::consensusMatrix(sequence_biostrings)

  x = x[rownames(x) != 'X', ]

  consensus = apply(x, 2, function(v) {
    if (all(v == 0)) {
      return('X')
    } else {
      return(rownames(x)[which.max(v)])
    }
  })

  return(paste(consensus, collapse = ''))
}


translate_with_deletions = function(s, reference_aas) {
  if (length(s) == 0) {
    return(c())
  }

  s_nodel = stringr::str_remove_all(s, fixed('-'))
  s_tr = Biostrings::DNAStringSet(s_nodel) %>%
    Biostrings::translate(if.fuzzy.codon = "X") %>%
    as.character()

  mafft_align(s_tr, reference_aas)
}


mafft_align = function(sequences, reference) {
  na_locs = is.na(sequences)

  tempdir = paste0(
    'mafft_temp_',
    paste(sample(c(LETTERS, letters), 10), collapse = '')
  )
  message(tempdir)
  dir.create(tempdir)

  on.exit({
    unlink(tempdir, recursive = T)
  })

  ref_file = paste0(tempdir, '/reference.fasta')
  unaln_file = paste0(tempdir, '/sequences.fasta')
  aln_file = paste0(tempdir, '/aligned.fasta')
  message(ref_file)
  message(unaln_file)
  message(aln_file)

  unq_sequences = unique(sequences)
  idxs = match(sequences, unq_sequences)
  message('Number to align = ', length(unq_sequences))

  message('Writing...')
  B_det = any(stringr::str_detect(unq_sequences, "B"))
  del_det = any(stringr::str_detect(unq_sequences, stringr::fixed("*")))

  if (B_det & del_det) {
    stop("cannot replace * with B because both are present")
  }
  if (del_det & !B_det) {
    unq_sequences = stringr::str_replace_all(
      unq_sequences,
      stringr::fixed("*"),
      "B"
    )
  }

  myUtils::fast_fasta.write(
    unq_sequences,
    names = seq_along(unq_sequences),
    path = unaln_file
  )

  myUtils::fast_fasta.write(reference, names = 'reference', path = ref_file)

  message('Aligning...')
  system(
    paste0(
      'mafft --thread 8 --quiet --6merpair --keeplength --addfragments ',
      unaln_file,
      ' ',
      ref_file,
      ' > ',
      aln_file
    )
  )

  message('Reading...')
  aligned_sequences = seqinr::read.fasta(aln_file, as.string = T)[-1]

  aligned_sequences_expanded = aligned_sequences[idxs]

  aligned_sequences_expanded[na_locs] = NA
  aligned_sequences_expanded = toupper(unlist(aligned_sequences_expanded))

  if (del_det & !B_det) {
    aligned_sequences_expanded = stringr::str_replace_all(
      aligned_sequences_expanded,
      "B",
      "*"
    )
  }

  setNames(
    aligned_sequences_expanded,
    names(sequences)
  )
}


ladderize = function(tree = NULL, file = NULL) {
  if (is.null(file) & is.null(tree)) {
    stop('Provide one of tree or file')
  }

  if (is.null(file)) {
    message('Writing tree')
    castor::write_tree(tree, file = 'tree.nwk')
    file = 'tree.nwk'
    on.exit(file.remove(file, file_reord))
  } else {
    on.exit(file.remove(file_reord))
  }

  file_reord = paste0(file, '.ord')

  PATH <- Sys.getenv("PATH")
  if (!stringr::str_detect(PATH, fixed("/Users/samturner/opt/anaconda3/bin"))) {
    Sys.setenv(
      PATH = paste(PATH, "/Users/samturner/opt/anaconda3/bin", sep = ":")
    )
  }

  system(paste0('nw_order -c n ', file, ' > ', file_reord))

  tr = ape::read.tree(file = file_reord)
  tr
}
