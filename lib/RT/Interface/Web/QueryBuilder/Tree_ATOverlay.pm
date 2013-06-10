
package RT::Interface::Web::QueryBuilder::Tree;

use strict;
no warnings qw(redefine);



=head2 GetReferencedTypes

Returns a hash reference; each type referenced with an '=' operation
will appear as a key whose value is 1.

=cut

sub GetReferencedTypes {
    my $self = shift;

    my $types = {};

    $self->traverse(
        sub {
            my $node = shift;

            return if $node->isRoot;
            return unless $node->isLeaf;

            my $clause = $node->getNodeValue();
            return unless $clause->{Key} eq 'Type';
            return unless $clause->{Op} eq '=';

            $types->{ $clause->{RawValue} } = 1;
        }
    );

    return $types;
}

sub ParseAssetSQL {
    my $self = shift;
    my %args = (
        Query => '',
        CurrentUser => '', #XXX: Hack
        @_
    );
    my $string = $args{'Query'};

    my @results;

    my %field = %{ RTx::AssetTracker::Assets->new( $args{'CurrentUser'} )->FIELDS };
    my %lcfield = map { ( lc($_) => $_ ) } keys %field;

    my $node =  $self;

    my %callback;
    $callback{'OpenParen'} = sub {
        $node = __PACKAGE__->new( 'AND', $node );
    };
    $callback{'CloseParen'} = sub { $node = $node->getParent };
    $callback{'EntryAggregator'} = sub { $node->setNodeValue( $_[0] ) };
    $callback{'Condition'} = sub {
        my ($key, $op, $value) = @_;
        my $rawvalue = $value;

        my ($main_key) = split /[.]/, $key;

        my $class;
        if ( exists $lcfield{ lc $main_key } ) {
            $key =~ s/^[^.]+/ $lcfield{ lc $main_key } /e;
            ($main_key) = split /[.]/, $key;  # make the case right
            $class = $field{ $main_key }->[0];
        }
        unless( $class ) {
            push @results, [ $args{'CurrentUser'}->loc("Unknown field: [_1]", $key), -1 ]
        }

        if ( lc $op eq 'is' || lc $op eq 'is not' ) {
            $value = 'NULL'; # just fix possible mistakes here
        } elsif ( $value !~ /^[+-]?[0-9]+$/ ) {
            $value =~ s/(['\\])/\\$1/g;
            $value = "'$value'";
        }

        if ($key =~ s/(['\\])/\\$1/g or $key =~ /[^{}\w\.]/) {
            $key = "'$key'";
        }

        my $clause = { Key => $key, Op => $op, Value => $value, RawValue => $rawvalue };
        $node->addChild( __PACKAGE__->new( $clause ) );
    };
    $callback{'Error'} = sub { push @results, @_ };

    require RT::SQL;
    RT::SQL::Parse($string, \%callback);
    return @results;
}

1;
