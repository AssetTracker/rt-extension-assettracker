# BEGIN BPS TAGGED BLOCK {{{
# 
# COPYRIGHT:
# 
# This software is Copyright (c) 1996-2010 Best Practical Solutions, LLC
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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 or visit their web page on the internet at
# http://www.gnu.org/licenses/old-licenses/gpl-2.0.html.
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

# Portions Copyright 2000 Tobias Brox <tobix@cpan.org> 

=head1 NAME

  RTx::AssetTracker::Template - RT's template object

=head1 SYNOPSIS

  use RTx::AssetTracker::Template;

=head1 DESCRIPTION


=head1 METHODS


=cut

use strict;
use warnings;

package RTx::AssetTracker::Template;
use base 'RT::Record';

sub Table {'AT_Templates'};

use RTx::AssetTracker::Asset;

use Text::Template;
use MIME::Entity;
use MIME::Parser;

sub _Accessible {
    my $self = shift;
    my %Cols = (
        id            => 'read',
        Name          => 'read/write',
        Description   => 'read/write',
        Type          => 'read/write',    #Type is one of Action or Message
        Content       => 'read/write',
        AssetType         => 'read/write',
        Creator       => 'read/auto',
        Created       => 'read/auto',
        LastUpdatedBy => 'read/auto',
        LastUpdated   => 'read/auto'
    );
    return $self->SUPER::_Accessible( @_, %Cols );
}

sub _Set {
    my $self = shift;
    
    unless ( $self->CurrentUserHasAssetTypeRight('ModifyTemplate') ) {
        return ( 0, $self->loc('Permission Denied') );
    }
    return $self->SUPER::_Set( @_ );
}

=head2 _Value

Takes the name of a table column. Returns its value as a string,
if the user passes an ACL check, otherwise returns undef.

=cut

sub _Value {
    my $self  = shift;

    unless ( $self->CurrentUserHasAssetTypeRight('ShowTemplate') ) {
        return undef;
    }
    return $self->__Value( @_ );

}

=head2 Load <identifer>

Load a template, either by number or by name.

Note that loading templates by name using this method B<is
ambiguous>. Several queues may have template with the same name
and as well global template with the same name may exist.
Use L</LoadGlobalTemplate> and/or L<LoadAssetTypeTemplate> to get
precise result.

=cut

sub Load {
    my $self       = shift;
    my $identifier = shift;
    return undef unless $identifier;

    if ( $identifier =~ /\D/ ) {
        return $self->LoadByCol( 'Name', $identifier );
    }
    return $self->LoadById( $identifier );
}

=head2 LoadGlobalTemplate NAME

Load the global template with the name NAME

=cut

sub LoadGlobalTemplate {
    my $self = shift;
    my $name = shift;

    return ( $self->LoadAssetTypeTemplate( AssetType => 0, Name => $name ) );
}

=head2 LoadAssetTypeTemplate (AssetType => TYPEID, Name => NAME)

Loads the AssetType template named NAME for Asset Type TYPE.

Note that this method doesn't load a global template with the same name
if template in the queue doesn't exist. The following code can be used:

    $template->LoadAssetTypeTemplate( AssetType => $type_id, Name => $template_name );
    unless ( $template->id ) {
        $template->LoadGlobalTemplate( $template_name );
        unless ( $template->id ) {
            # no template
            ...
        }
    }
    # ok, template either type's or global
    ...

=cut

sub LoadAssetTypeTemplate {
    my $self = shift;
    my %args = (
        AssetType => undef,
        Name  => undef,
        @_
    );

    return ( $self->LoadByCols( Name => $args{'Name'}, AssetType => $args{'AssetType'} ) );

}

=head2 Create

Takes a paramhash of Content, AssetType, Name and Description.
Name should be a unique string identifying this Template.
Description and Content should be the template's title and content.
AssetType should be 0 for a global template and the queue # for a queue-specific 
template.

Returns the Template's id # if the create was successful. Returns undef for
unknown database failure.

=cut

sub Create {
    my $self = shift;
    my %args = (
        Content     => undef,
        AssetType       => 0,
        Description => '[no description]',
        Type        => 'Action', #By default, template are 'Action' templates
        Name        => undef,
        @_
    );

    unless ( $args{'AssetType'} ) {
        unless ( $self->CurrentUser->HasRight(Right =>'ModifyTemplate', Object => $RTx::AssetTracker::System) ) {
            return ( undef, $self->loc('Permission Denied') );
        }
        $args{'AssetType'} = 0;
    }
    else {
        my $TypeObj = new RTx::AssetTracker::Type( $self->CurrentUser );
        $TypeObj->Load( $args{'AssetType'} ) || return ( undef, $self->loc('Invalid asset type') );
    
        unless ( $TypeObj->CurrentUserHasRight('ModifyTemplate') ) {
            return ( undef, $self->loc('Permission Denied') );
        }
        $args{'AssetType'} = $TypeObj->Id;
    }

    my $result = $self->SUPER::Create(
        Content     => $args{'Content'},
        AssetType       => $args{'AssetType'},
        Description => $args{'Description'},
        Name        => $args{'Name'},
    );

    return ($result);

}

=head2 Delete

Delete this template.

=cut

sub Delete {
    my $self = shift;

    unless ( $self->CurrentUserHasAssetTypeRight('ModifyTemplate') ) {
        return ( 0, $self->loc('Permission Denied') );
    }

    return ( $self->SUPER::Delete(@_) );
}

=head2 IsEmpty

Returns true value if content of the template is empty, otherwise
returns false.

=cut

sub IsEmpty {
    my $self = shift;
    my $content = $self->Content;
    return 0 if defined $content && length $content;
    return 1;
}

=head2 MIMEObj

Returns L<MIME::Entity> object parsed using L</Parse> method. Returns
undef if last call to L</Parse> failed or never be called.

Note that content of the template is UTF-8, but L<MIME::Parser> is not
good at handling it and all data of the entity should be treated as
octets and converted to perl strings using Encode::decode_utf8 or
something else.

=cut

sub MIMEObj {
    my $self = shift;
    return ( $self->{'MIMEObj'} );
}

=head2 Parse

This routine performs L<Text::Template> parsing on the template and then
imports the results into a L<MIME::Entity> so we can really use it. Use
L</MIMEObj> method to get the L<MIME::Entity> object.

Takes a hash containing Argument, AssetObj, and TransactionObj and other
arguments that will be available in the template's code. AssetObj and
TransactionObj are not mandatory, but highly recommended.

It returns a tuple of (val, message). If val is false, the message contains
an error message.

=cut

sub Parse {
    my $self = shift;
    my ($rv, $msg);


    if ($self->Content =~ m{^Content-Type:\s+text/html\b}im) {
        local $RT::Transaction::PreferredContentType = 'text/html';
        ($rv, $msg) = $self->_Parse(@_);
    }
    else {
        ($rv, $msg) = $self->_Parse(@_);
    }

    return ($rv, $msg) unless $rv;

    my $mime_type   = $self->MIMEObj->mime_type;
    if (defined $mime_type and $mime_type eq 'text/html') {
        $self->_DowngradeFromHTML(@_);
    }

    return ($rv, $msg);
}

sub _Parse {
    my $self = shift;

    # clear prev MIME object
    $self->{'MIMEObj'} = undef;

    #We're passing in whatever we were passed. it's destined for _ParseContent
    my ($content, $msg) = $self->_ParseContent(@_);
    return ( 0, $msg ) unless defined $content && length $content;

    if ( $content =~ /^\S/s && $content !~ /^\S+:/ ) {
        $RT::Logger->error(
            "Template #". $self->id ." has leading line that doesn't"
            ." look like header field, if you don't want to override"
            ." any headers and don't want to see this error message"
            ." then leave first line of the template empty"
        );
        $content = "\n".$content;
    }

    my $parser = MIME::Parser->new();
    $parser->output_to_core(1);
    $parser->tmp_to_core(1);
    $parser->use_inner_files(1);

    ### Should we forgive normally-fatal errors?
    $parser->ignore_errors(1);
    # MIME::Parser doesn't play well with perl strings
    utf8::encode($content);
    $self->{'MIMEObj'} = eval { $parser->parse_data( \$content ) };
    if ( my $error = $@ || $parser->last_error ) {
        $RT::Logger->error( "$error" );
        return ( 0, $error );
    }

    # Unfold all headers
    $self->{'MIMEObj'}->head->unfold;

    return ( 1, $self->loc("Template parsed") );

}

# Perform Template substitutions on the template

sub _ParseContent {
    my $self = shift;
    my %args = (
        Argument       => undef,
        AssetObj      => undef,
        TransactionObj => undef,
        @_
    );

    unless ( $self->CurrentUserHasAssetTypeRight('ShowTemplate') ) {
        return (undef, $self->loc("Permission Denied"));
    }

    if ( $self->IsEmpty ) {
        return ( undef, $self->loc("Template is empty") );
    }

    my $content = $self->SUPER::_Value('Content');
    # We need to untaint the content of the template, since we'll be working
    # with it
    $content =~ s/^(.*)$/$1/;
    my $template = Text::Template->new(
        TYPE   => 'STRING',
        SOURCE => $content
    );

    $args{'Asset'} = delete $args{'AssetObj'} if $args{'AssetObj'};
    $args{'Transaction'} = delete $args{'TransactionObj'} if $args{'TransactionObj'};
    #$args{'Requestor'} = eval { $args{'Ticket'}->Requestors->UserMembersObj->First->Name }
        #if $args{'Ticket'};
    $args{'rtname'}    = RT->Config->Get('rtname');
    if ( $args{'Asset'} ) {
        my $t = $args{'Asset'}; # avoid memory leak
        $args{'loc'} = sub { $t->loc(@_) };
    } else {
        $args{'loc'} = sub { $self->loc(@_) };
    }

    foreach my $key ( keys %args ) {
        next unless ref $args{ $key };
        next if ref $args{ $key } =~ /^(ARRAY|HASH|SCALAR|CODE)$/;
        my $val = $args{ $key };
        $args{ $key } = \$val;
    }


    my $is_broken = 0;
    my $retval = $template->fill_in(
        HASH => \%args,
        BROKEN => sub {
            my (%args) = @_;
            $RT::Logger->error("Template parsing error: $args{error}")
                unless $args{error} =~ /^Died at /; # ignore intentional die()
            $is_broken++;
            return undef;
        }, 
    );
    return ( undef, $self->loc('Template parsing error') ) if $is_broken;

    return ($retval);
}

sub _DowngradeFromHTML {
    my $self = shift;
    my $orig_entity = $self->MIMEObj;

    my $new_entity = $orig_entity->dup; # this will fail badly if we go away from InCore parsing
    $new_entity->head->mime_attr( "Content-Type" => 'text/plain' );
    $new_entity->head->mime_attr( "Content-Type.charset" => 'utf-8' );

    $orig_entity->head->mime_attr( "Content-Type" => 'text/html' );
    $orig_entity->head->mime_attr( "Content-Type.charset" => 'utf-8' );
    $orig_entity->make_multipart('alternative', Force => 1);

    require HTML::FormatText;
    require HTML::TreeBuilder;
    require Encode;
    # need to decode_utf8, see the doc of MIMEObj method
    my $tree = HTML::TreeBuilder->new_from_content(
        Encode::decode_utf8($new_entity->bodyhandle->as_string)
    );
    $new_entity->bodyhandle(MIME::Body::InCore->new(
        \(scalar HTML::FormatText->new(
            leftmargin  => 0,
            rightmargin => 78,
        )->format( $tree ))
    ));
    $tree->delete;

    $orig_entity->add_part($new_entity, 0); # plain comes before html
    $self->{MIMEObj} = $orig_entity;

    return;
}

=head2 CurrentUserHasAssetTypeRight

Helper function to call the template's queue's CurrentUserHasAssetTypeRight with the passed in args.

=cut

sub CurrentUserHasAssetTypeRight {
    my $self = shift;
    return ( $self->AssetTypeObj->CurrentUserHasRight(@_) );
}



=head2 id

Returns the current value of id. 
(In the database, id is stored as int(11).)


=cut


=head2 AssetType

Returns the current value of AssetType. 
(In the database, AssetType is stored as int(11).)



=head2 SetAssetType VALUE


Set AssetType to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, AssetType will be stored as a int(11).)


=cut


=head2 AssetTypeObj

Returns the AssetType Object which has the id returned by AssetType


=cut

sub AssetTypeObj {
	my $self = shift;
	my $Type =  RTx::AssetTracker::Type->new($self->CurrentUser);
	$Type->Load($self->__Value('AssetType'));
	return($Type);
}

=head2 Name

Returns the current value of Name. 
(In the database, Name is stored as varchar(200).)



=head2 SetName VALUE


Set Name to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Name will be stored as a varchar(200).)


=cut


=head2 Description

Returns the current value of Description. 
(In the database, Description is stored as varchar(255).)



=head2 SetDescription VALUE


Set Description to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Description will be stored as a varchar(255).)


=cut


=head2 Type

Returns the current value of Type. 
(In the database, Type is stored as varchar(16).)



=head2 SetType VALUE


Set Type to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Type will be stored as a varchar(16).)


=cut


=head2 Language

Returns the current value of Language. 
(In the database, Language is stored as varchar(16).)



=head2 SetLanguage VALUE


Set Language to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Language will be stored as a varchar(16).)


=cut


=head2 TranslationOf

Returns the current value of TranslationOf. 
(In the database, TranslationOf is stored as int(11).)



=head2 SetTranslationOf VALUE


Set TranslationOf to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, TranslationOf will be stored as a int(11).)


=cut


=head2 Content

Returns the current value of Content. 
(In the database, Content is stored as blob.)



=head2 SetContent VALUE


Set Content to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Content will be stored as a blob.)


=cut


=head2 LastUpdated

Returns the current value of LastUpdated. 
(In the database, LastUpdated is stored as datetime.)


=cut


=head2 LastUpdatedBy

Returns the current value of LastUpdatedBy. 
(In the database, LastUpdatedBy is stored as int(11).)


=cut


=head2 Creator

Returns the current value of Creator. 
(In the database, Creator is stored as int(11).)


=cut


=head2 Created

Returns the current value of Created. 
(In the database, Created is stored as datetime.)


=cut



sub _CoreAccessible {
    {
     
        id =>
		{read => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => ''},
        AssetType => 
		{read => 1, write => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Name => 
		{read => 1, write => 1, sql_type => 12, length => 200,  is_blob => 0,  is_numeric => 0,  type => 'varchar(200)', default => ''},
        Description => 
		{read => 1, write => 1, sql_type => 12, length => 255,  is_blob => 0,  is_numeric => 0,  type => 'varchar(255)', default => ''},
        Type => 
		{read => 1, write => 1, sql_type => 12, length => 16,  is_blob => 0,  is_numeric => 0,  type => 'varchar(16)', default => ''},
        Language => 
		{read => 1, write => 1, sql_type => 12, length => 16,  is_blob => 0,  is_numeric => 0,  type => 'varchar(16)', default => ''},
        TranslationOf => 
		{read => 1, write => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Content => 
		{read => 1, write => 1, sql_type => -4, length => 0,  is_blob => 1,  is_numeric => 0,  type => 'blob', default => ''},
        LastUpdated => 
		{read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},
        LastUpdatedBy => 
		{read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Creator => 
		{read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Created => 
		{read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},

 }
};

RT::Base->_ImportOverlays();

1;
