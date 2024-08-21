#!/usr/bin/perl

#*******************************************************************************
# Given a list of Fortran 90 object files and include files this script writes
# to standard output, for each corresponding object file, the Makefile
# dependencies on modules and include files in the form:
#
#     file.o: dependency_1.o dependency_2.o ... dependency_3.inc ...
#
# It is assumed that dependencies on source files of the following form are
# implicitly defined as is usually the case:
#
#     file.o: file.f90
#
# Usage: make_depend.pl [object files] [include files]
#
# ... where object files have a '.o' extension and include files have a '.inc'
# extension.
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
# 2015/11/13, Greg McGarragh: Added support for includes in .f, .F, .for, .FOR,
#    and .f90 files and added support for modules in .f90 files.
# 2016/08/10, Greg McGarragh: Add support for source files in subdirectories
#    specified with one or more --subdir <path> options.
#
#*******************************************************************************
use strict;

use File::Basename;
use Getopt::Long;

my $ext_regex_f77   = "f|for";
my $ext_regex_f90   = "f90|F90";
my $ext_regex_fxx   = "$ext_regex_f77|$ext_regex_f90";
my $objects_path    = "\$(OBJS)/"; # Path where the object files are to be located
my $includes_path   = "";          # Path where the include files are to be located
my $indent_length   = 8;
my $max_line_length = 80;

my @source_file_list;
my @include_file_list;
my @include_file_list;

my @subdirs;
my @dir_list;

my @dependencies2;

my %module_to_mod_base;

# Filter out and parse options
GetOptions("subdir=s" => \@subdirs) || die("ERROR: Error in command line arguments\n");

# Process command line arguments
foreach (@ARGV) {
    if    (index($_, "\.o")   != -1) { push(@source_file_list,  $_); }
    elsif (index($_, "\.inc") != -1) { push(@include_file_list, $_); }
}

# Strip paths from the object files and change from object to source extensions
opendir(dir, '.');
@dir_list = readdir(dir);
closedir(dir);

foreach my $subdir (@subdirs) {
    opendir(dir, $subdir) || die("ERROR: Subdirectory not found: $subdir\n");
    while (readdir(dir)) {
        push(@dir_list, "$subdir/$_");
    }
    closedir(dir);
}

foreach (@source_file_list) {
    s{.*/}{};
    s/\.o//;
    my $regex = "(^|/)$_\.($ext_regex_fxx)\$";
    my @filelist = grep(/$regex/, @dir_list);
    if (scalar @filelist == 0) {
        die("ERROR: No source file found for base name: $_");
    }
    if (scalar @filelist >  1) {
        die("ERROR: More than one extension for base name: $_");
    }
    $_ = $filelist[0];
}

# Strip paths from the include files
foreach (@include_file_list) {
    s/(.*)\/.*$/$1/
}

# Map module names to their associated source files
foreach my $source_file (@source_file_list) {
    open(FILE, $source_file) ||
        die("Unable to open source file: $source_file");
    while (<FILE>) {
        /^\s*module\s+([^\s!]+)/i &&
        ($module_to_mod_base{lc($1)} = $source_file) =~ s/\.($ext_regex_fxx)$//;
    }
    close(FILE);
}

# Write dependencies for each source file to standard output
foreach my $source_file (@source_file_list) {
    my $object_file = $source_file;
    $object_file =~ s/.*\///;
    $object_file =~ s/\.($ext_regex_fxx)$/.o/;

    get_file_depencies($source_file, $source_file, $object_file);

    # Pretty print the dependencies for the Makefile
    if (@dependencies2) {
        my $line_length;

        @dependencies2 = &uniq(sort(@dependencies2));

        print "$objects_path$object_file:";

        $line_length = length($objects_path . $object_file) + 1;

        foreach my $dependency (@dependencies2) {
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
    my $source_file  = $_[0];
    my $current_file = $_[1];
    my $object_file  = $_[2];

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
            if ("$_.f" ~~ @source_file_list or "$_.f90" ~~ @source_file_list or "$_.F90" ~~ @source_file_list) {
                s{.*/}{};
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
                get_file_depencies($source_file, $_, $object_file)
            }

            if ($_ ~~ @include_file_list) {
                get_file_depencies($source_file, $_, $object_file)
            }
        }

        undef @modules;
        undef @includes;
        undef @mod_bases;
        undef @dependencies;
    }
}
