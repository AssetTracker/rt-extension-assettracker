package RTx::ATLinkCompletion;

use strict;
require RTx::AssetTracker::Assets;
require RTx::AssetTracker::Type;

our $VERSION = "0.01";

sub search_rdbms {
    my $AssetLink = shift;
    my $CurrentUser = shift;

    return unless $CurrentUser->Privileged() or defined $RT::ATLinkCompletionUnprivileged;

    my @links;

    my $Assets = RTx::AssetTracker::Assets->new( $CurrentUser );
    my $Types = RTx::AssetTracker::Types->new( $CurrentUser );
    $Types->UnLimit();

    foreach my $field (@{$RT::ATLinkCompletionSearchFields}) {
	$Assets->Limit(SUBCLAUSE => 'ATLinkCompletion', ALIAS => 'main', FIELD => $field, OPERATOR => $RT::ATLinkCompletionSearch, VALUE => $AssetLink, ENTRYAGGREGATOR => 'OR');
    }
 
    $RT::Logger->debug($Assets->BuildSelectQuery);

    my @assets;
    while (my $Asset = $Assets->Next) {
	push @assets, $Asset;
    }

    my @types;
    while (my $Type = $Types->Next) {
	$types[$Type->Id()] = $Type->Name();
    }

    my @link = map { [ $types[$_->Type], $_->Name(), $_->URI() ] } @assets;
}

# we dynamically build search function

our $AUTOLOAD;
sub AUTOLOAD {
    (my $function = $AUTOLOAD) =~ s/.*:://;
    die "Unable to find search function in AUTOLOAD" unless $function eq 'search';
    
    my $str = 'sub search { my (@assets);';
    $str   .= '@assets = search_rdbms(@_);';
    $str   .= 'return (\@assets); }';

    eval $str;
    goto &search;
}


1;
