#!/usr/bin/perl

#*******************************************************************************
# Given a list of Fortran 90 object files and include files this script writes
# to standard output, for each corresponding object file, the Makefile
# dependencies on modules and include files in the form:
#
#	file.o: dependency_1.o dependency_2.o ... dependency_3.inc ...
#
# It is assumed that dependencies on source files of the following form are
# implicitly defined as is usually the case:
#
#	file.o: file.F90
#
# Usage: make_f90_depend.pl [object files] [include files]
#
# ... where object files have a '.o' extension and include files have a '.inc'
# extension.
#
# History
# 2013/12/04, Greg McGarragh: Original version
#
#*******************************************************************************

$objects_path    = "\$(OBJS)/";	# Path where the object files are to be located
$includes_path   = "";		# Path where the include files are to be located
$indent_length   = 8;
$max_line_length = 80;

# Read command line arguments
foreach (@ARGV) {
	if    (index($_, "\.o")   != -1) { push(@source_file_list,  $_); }
	elsif (index($_, "\.inc") != -1) { push(@include_file_list, $_); }
	elsif (index($_, "\.F90") != -1) { push(@include_file_list, $_); }
}

# Change from object to source extensions and strip paths from the object and
# include files
foreach (@source_file_list) {
	s{.*/}{};
	s/\.o/.F90/;
}
foreach (@include_file_list) {
	s/(.*)\/.*$/$1/
}

# Map module names to their associated source files
foreach $source_file (@source_file_list) {
	open(FILE, $source_file) ||
		die("Unable to open source file: $source_file");
	while (<FILE>) {
		/^\s*module\s+([^\s!]+)/i &&
			($module_to_mod_base{lc($1)} = $source_file) =~ s/\.F90//;
	}
	close(FILE);
}

# Like UNIX uniq except operates on a list and returns a new list
sub uniq {
	local(@words);
	foreach $word (@_) {
		if ($word ne $words[$#words]) {
			push(@words, $word);
		}
	}
	@words;
}

# Write dependencies for each source file to standard output
foreach $source_file (@source_file_list) {
	open(FILE, $source_file) or
		die("Unable to open source file: $source_file");

        # Find used modules and included files
	while (<FILE>) {
		/^\s*use\s+([^\s,!]+)/i              && push(@modules, lc($1));
		/^\s*include\s+["\']([^"\']+)["\']/i && push(@includes,   $1 );
	}

	close(FILE);

	if (defined @includes || defined @modules) {
		$object_file = $source_file;
		$object_file =~ s/\.F90/.o/;

		# Apply module to file mapping
		foreach (@modules) {
			push(@mod_bases, $module_to_mod_base{$_});
		}

		# Get rid of duplicates from multiple uses in the file
		@mod_bases = &uniq(sort(@mod_bases));

		# If the module is local include it as a dependency
		foreach (@mod_bases) {
			if("$_.F90" ~~ @source_file_list) {
				push(@dependencies, "$objects_path$_.o");
			}
		}

		# Get rid of duplicates from multiple includes in the file
		@includes = &uniq(sort(@includes));

		# If the include is local include it as a dependency
		foreach (@includes) {
			if("$_" ~~ @include_file_list) {
				push(@dependencies, "$includes_path$_");
			}
		}

		# Get rid of circular dependencies
		foreach (@dependencies) {
			if ("$objects_path$object_file" ne "$_") {
				push(@dependencies2, $_);
			}
		}

		# Pretty print the dependencies for the Makefile
		if (defined @dependencies2) {
			print "$objects_path$object_file:";

			$length = length($objects_path . $object_file) + 1;

			foreach (@dependencies2) {
				$length += 1 + length($_);

				if ($length > $max_line_length) {
					print " \\\n";
					print " " x ($indent_length - 1);
					$length = $indent_length + length($_);
				}

				print " $_"
			}
		}

		print "\n";

		undef @mod_bases;
		undef @dependencies;
		undef @dependencies2;
	}

	undef @modules;
	undef @includes;
}
