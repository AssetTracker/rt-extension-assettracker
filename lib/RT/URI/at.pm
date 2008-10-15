# BEGIN BPS TAGGED BLOCK {{{
# 
# COPYRIGHT:
#  
# This software is Copyright (c) 1996-2004 Best Practical Solutions, LLC 
#                                          <jesse@bestpractical.com>
# 
# (Except where explicitly superseded by other copyright notices)
# 
# 
# LICENSE:
# 
# This work is made available to you under the terms of Version 2 of
# the GNU General Public License. A copy of that license should have
# been provided with this software, but in any event can be snarfed
# from www.gnu.org.
# 
# This work is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
# 
# 
# CONTRIBUTION SUBMISSION POLICY:
# 
# (The following paragraph is not intended to limit the rights granted
# to you to modify and distribute this software under the terms of
# the GNU General Public License and is only of importance to you if
# you choose to contribute your changes and enhancements to the
# community by submitting them to Best Practical Solutions, LLC.)
# 
# By intentionally submitting any modifications, corrections or
# derivatives to this work, or any other work intended for use with
# Request Tracker, to Best Practical Solutions, LLC, you confirm that
# you are the copyright holder for those contributions and you grant
# Best Practical Solutions,  LLC a nonexclusive, worldwide, irrevocable,
# royalty-free, perpetual, license to use, copy, create derivative
# works based on those contributions, and sublicense and distribute
# those contributions and any derivatives thereof.
# 
# END BPS TAGGED BLOCK }}}
package RT::URI::at;

use RTx::AssetTracker::Asset;

use RT::URI::base;

use strict;
use vars qw(@ISA);
@ISA = qw/RT::URI::base/;




=head2 LocalURIPrefix  

Returns the prefix for a local URI. 

=begin testing

use_ok("RT::URI::at");
my $uri = RT::URI::at->new($RT::SystemUser);

ok(ref($uri), "\$uri is a reference");

use Data::Dumper;


ok (UNIVERSAL::isa($uri,RT::URI::at), "It's an RT::URI::at");

ok ($uri->isa('RT::URI::base'), "It's an RT::URI::base");
ok ($uri->isa('RT::Base'), "It's an RT::Base");

is ($uri->LocalURIPrefix , 'at://example.com', 'LocalURIPrefix checks out');

=end testing



=cut

sub LocalURIPrefix {
    my $self = shift;
    
    my $prefix = $self->Scheme. "://$RT::Organization";

    return ($prefix);
}

=head2 ObjectType

=cut

sub ObjectType {
    my $self = shift;
    my $object = shift || $self->Object;

    my $type = 'asset';
    if (ref($object) && (ref($object) ne 'RTx::AssetTracker::Asset')) {
            $type = ref($object);
    }

    return ($type);
}




=head2 URIForObject RT::Record

Returns the RT URI for a local RT::Record object

=begin testing

my $asset = RTx::AssetTracker::Asset->new($RT::SystemUser);
$asset->Create(Type => 'Servers', Name => 'URI test');

$asset = RTx::AssetTracker::Asset->new($RT::SystemUser);
$asset->Load(1);
my $uri = RT::URI::at->new($asset->CurrentUser);
is($uri->LocalURIPrefix. "/asset/1" , $uri->URIForObject($asset), 'URIForObject checks out');

=end testing

=cut

sub URIForObject {
    my $self = shift;
    my $obj = shift;
    return ($self->LocalURIPrefix."/".$self->ObjectType($obj)."/". $obj->Id);
}


=head2 ParseURI URI

When handed an fsck.com-rt: URI, figures out things like whether its a local record and what its ID is

=cut


sub ParseURI {
    my $self = shift;
    my $uri  = shift;

    if ( $uri =~ /^(\d+)$/ ) {
        my $asset = RTx::AssetTracker::Asset->new( $self->CurrentUser );
        $asset->Load($uri);
        $self->{'uri'} = $asset->URI;
        $self->{'object'} = $asset;
        return($asset->id);
    }
    else {
        $self->{'uri'} = $uri;
    }

    #If it's a local URI, load the asset object and return its URI
    if ( $self->IsLocal ) {

        my $local_uri_prefix = $self->LocalURIPrefix;
        if ( $self->{'uri'} =~ /^$local_uri_prefix\/(.*?)\/(\d+)$/i ) {
            my $type = $1;
            my $id   = $2;

            if ( $type eq 'asset' ) { $type = 'RTx::AssetTracker::Asset' }

            # We can instantiate any RT::Record subtype. but not anything else

            if ( UNIVERSAL::isa( $type, 'RTx::AssetTracker::Record' ) ) {
                my $record = $type->new( $self->CurrentUser );
                $record->Load($id);

                if ( $record->Id ) {
                    $self->{'object'} = $record;
                    return ( $record->Id );
                }
            }

        }
    }
    return undef;
}

=head2 IsLocal 

Returns true if this URI is for a local asset.
Returns undef otherwise.



=cut

sub IsLocal {
	my $self = shift;
        my $local_uri_prefix = $self->LocalURIPrefix;
	if ($self->{'uri'} =~ /^$local_uri_prefix/i) {
		return 1;
    }
	else {
		return undef;
	}
}



=head2 Object

Returns the object for this URI, if it's local. Otherwise returns undef.

=cut

sub Object {
    my $self = shift;
    return ($self->{'object'});

}

=head2 Scheme

Return the URI scheme for RT records

=cut


sub Scheme {
    my $self = shift;
	return "at";
}

=head2 HREF

If this is a local asset, return an HTTP url to it.
Otherwise, return its URI

=cut


sub HREF {
    my $self = shift;
    if ($self->IsLocal && $self->Object && ($self->ObjectType eq 'asset')) {
        return ( $RT::WebURL . "AssetTracker/Asset/Display.html?id=".$self->Object->Id);
    }   
    else {
        return ($self->URI);
    }
}

=head2 AsString

Returns either a localized string 'asset #23' or the full URI if the object is not local

=cut

sub AsString {
    my $self = shift;
    if ($self->IsLocal && $self->Object) {
	    return $self->loc("[_1] #[_2] " . $self->Object->Name, $self->ObjectType, $self->Object->Id);
    }
    else {
	    return $self->URI;
    }
}

eval "require RT::URI::at_Vendor";
die $@ if ($@ && $@ !~ qr{^Can't locate RT/URI/at_Vendor.pm});
eval "require RT::URI::at_Local";
die $@ if ($@ && $@ !~ qr{^Can't locate RT/URI/at_Local.pm});

1;
