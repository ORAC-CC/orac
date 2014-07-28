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
# or .F90 extension.
#
# History
# 2013/12/04, Greg McGarragh: Original version
# 2014/05/06, Greg McGarragh: Add support to recursively check include files for
#    dependencies.
# 2014/05/06, Greg McGarragh: Modified to assume that include files that exist
#    in the current directory are local and subject to be a dependency whereas
#    include files that do not exist in the current directory are assumed to be
#    external and therefore not subject to be a dependency.
# 2014/05/26, Greg McGarragh: Added support for C preprocessor includes.
# 2014/07/27, Greg McGarragh: Removed use of "defined" which is depricated in
#    Perl 5, version 18.  Added "use strict" and implemented explicit variable
#    declarations required by use strict.  Finally, some rearranging.
#
#*******************************************************************************
use strict;

use File::Basename;

my $objects_path    = "\$(OBJS)/";	# Path where the object files are to be located
my $includes_path   = "";		# Path where the include files are to be located
my $indent_length   = 8;
my $max_line_length = 80;

my $source_file;
my $object_file;
my $dependency;

my @source_file_list;
my @include_file_list;
my @include_file_list;

my @dependencies2;

my %module_to_mod_base;

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

# Write dependencies for each source file to standard output
foreach $source_file (@source_file_list) {
	$object_file = $source_file;
	$object_file =~ s/\.F90/.o/;

	get_file_depencies($source_file, $source_file);

	# Pretty print the dependencies for the Makefile
	if (@dependencies2) {
                my $line_length;

		@dependencies2 = &uniq(sort(@dependencies2));

		$object_file = $source_file;
		$object_file =~ s/\.F90/.o/;

		print "$objects_path$object_file:";

		$line_length = length($objects_path . $object_file) + 1;

		foreach $dependency (@dependencies2) {
			$line_length += 1 + length($dependency);

			if ($line_length > $max_line_length) {
				print " \\\n";
				print " " x ($indent_length - 1);
				$line_length = $indent_length + length($dependency);
			}

			print " $dependency"
		}

		print "\n";

		undef @dependencies2;
	}
}

# Like UNIX uniq except operates on a list and returns a new list
sub uniq {
	my @words;

	foreach (@_) {
		if ($_ ne $words[$#words]) {
			push(@words, $_);
		}
	}

	@words;
}

# Find dependencies for $current_file and write them as dependencies for
# $sourcefile.  Recursively called for all included files in $current_file.
sub get_file_depencies {
	my $source_file   = $_[0];
	my $current_file  = $_[1];

	my @modules;
	my @includes;
	my @mod_bases;
	my @dependencies;

	open(FILE, $current_file) or
		die("Unable to open source file: $current_file");

        # Find used modules and included files
	while (<FILE>) {
		/^\s*use\s+([^\s,!]+)/i                 && push(@modules, lc($1));
		/^\#*\s*include\s+["\']([^"\']+)["\']/i && push(@includes,   $1 );
	}

	close(FILE);

	if (@modules || @includes) {
		# Apply module to file mapping
		foreach (@modules) {
			push(@mod_bases, $module_to_mod_base{$_});
		}

		# Get rid of duplicates from multiple uses in the file
		@mod_bases = &uniq(sort(@mod_bases));

		# If the module is local include it as a dependency
		foreach (@mod_bases) {
			if ("$_.F90" ~~ @source_file_list) {
				push(@dependencies, "$objects_path$_.o");
			}
		}

		# Get rid of duplicates from multiple includes in the file
		@includes = &uniq(sort(@includes));

		# If the include is local include it as a dependency
		foreach (@includes) {
			if (-e basename($_)) {
				push(@dependencies, "$includes_path$_");
                        }

			if ($_ ~~ @include_file_list) {
				push(@dependencies, "$includes_path$_");
			}
		}

		# Get rid of circular dependencies
		foreach (@dependencies) {
			if ("$objects_path$object_file" ne "$_") {
				push(@dependencies2, $_);
			}
		}

		# Call this subroutine for all included files passing along
		# current list of dependencies
		foreach (@includes) {
			if (-e basename($_)) {
				get_file_depencies($source_file, $_)
                        }

			if ($_ ~~ @include_file_list) {
				get_file_depencies($source_file, $_)
			}
		}

		undef @modules;
		undef @includes;
		undef @mod_bases;
		undef @dependencies;
	}
}
