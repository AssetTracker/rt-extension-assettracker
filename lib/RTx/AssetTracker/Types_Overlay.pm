=head1 NAME

  RTx::AssetTracker::Types - a collection of AssetTracker Types objects

=head1 SYNOPSIS

  use RTx::AssetTracker::Types;

=head1 DESCRIPTION


=head1 METHODS

=cut


package RTx::AssetTracker::Types;

use strict;
no warnings qw(redefine);


# {{{ sub _Init
sub _Init {
  my $self = shift;
  $self->{'table'} = "AT_Types";
  $self->{'primary_key'} = "id";

  # By default, order by name
  $self->OrderBy( ALIAS => 'main',
                  FIELD => 'Name',
                  ORDER => 'ASC');

  return ($self->SUPER::_Init(@_));
}
# }}}

# {{{ sub _DoSearch

=head2 _DoSearch

  A subclass of DBIx::SearchBuilder::_DoSearch that makes sure that _Disabled rows never get seen unless
we're explicitly trying to see them.

=cut

sub _DoSearch {
    my $self = shift;

    #unless we really want to find disabled rows, make sure we\'re only finding enabled ones.
    unless($self->{'find_disabled_rows'}) {
        $self->LimitToEnabled();
    }

    return($self->SUPER::_DoSearch(@_));

}

# }}}


# {{{ sub Limit
sub Limit  {
  my $self = shift;
  my %args = ( ENTRYAGGREGATOR => 'AND',
               @_);
  $self->SUPER::Limit(%args);
}
# }}}

# {{{ sub Next

=head2 Next

Returns the next type that this user can see.

=cut

sub Next {
    my $self = shift;


    my $Type = $self->SUPER::Next();
    if ((defined($Type)) and (ref($Type))) {

        if ($Type->CurrentUserHasRight('SeeType')) {
            return($Type);
        }

        #If the user doesn't have the right to show this type
        else {
            return($self->Next());
        }
    }
    #if there never was any type
    else {
        return(undef);
    }

}
# }}}


1;
