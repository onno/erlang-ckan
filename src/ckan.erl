%%%--------------------------------------------------------------------------
%%% @author Onno van Zinderen Bakker <onno01@chello.nl>
%%% @copyright 2011 Onno van Zinderen Bakker. All rights reserved.
%%%
%%% @doc Erlang wrapper around the CKAN API.
%%%      The Comprehensive Knowledge Archive Network (CKAN) is is a 
%%%      web-based system for the storage and distribution of data, such as 
%%%      spreadsheets and the contents of databases supported by the 
%%%      Open Knowledge Foundation. It is inspired by the package management 
%%%      capabilities common to open source operating systems like Linux, 
%%%      and is intended to be the "apt-get of Debian for data".
%%%
%%%      The system is used both as a public platform on ckan.net and in 
%%%      various government data catalogues, such as the UK's [data.gov.uk], 
%%%      Norway's [data.norge.no] and the Dutch national data register.
%%%
%%%      The ckan erlang module allows readonly access to the REST API of 
%%%      a CKAN repository. At this moment there is no support for
%%%      adding or updating. This module uses version 2 of the CKAN API.
%%% @end
%%%--------------------------------------------------------------------------

-module(ckan).
-author('Onno van Zinderen Bakker <onno01@chello.nl>').

-export([start/0, stop/0]).
-export([packages/0, 
         packages/1, 
         package/1, 
         package/2,
         groups/0, 
         groups/1,
         group/1, 
         group/2,
         tags/0, 
         tags/1,
         packages_with_tag/1, 
         packages_with_tag/2,
         package_revisions/1,
         package_revisions/2,
         revisions/0, 
         revisions/1,
         revision/1,
         revision/2]).

-define(DEFAULT_REPOSITORY, "http://ckan.net").
-define(API_PATH, "api/2/rest").


%%%==========================================================================
%%% Application start / stop
%%%==========================================================================

%% @spec start() -> ok
%% @doc Start the socialnews server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:start(ckan).

%% @spec stop() -> ok
%% @doc Stop the ckan server.
stop() ->
    Res = application:stop(ckan),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%%%==========================================================================
%%% Model API
%%%==========================================================================

%%--------------------------------------------------------------------------
%% @doc Gets a list of all package id's available at [http://ckan.net]
%% @spec packages() -> {ok, List} | {error, Reason}
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
packages() ->
    packages(?DEFAULT_REPOSITORY).

%%--------------------------------------------------------------------------
%% @doc Gets a list of all the package names available at a CKAN repository.
%% @spec packages(RepositoryUrl) -> {ok, List} | {error, Reason}
%% where 
%% @end
%%--------------------------------------------------------------------------
packages(RepositoryUrl) ->
    rest:get_json(resource_url(RepositoryUrl, ["package"])).

%%--------------------------------------------------------------------------
%% @doc Gets the metadata of a single package at [http://ckan.net]
%% @spec package(PackageId) -> {ok, PackageInfo} | {error, Reason}
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
package(PackageId) ->
    package(?DEFAULT_REPOSITORY, PackageId).

%%--------------------------------------------------------------------------
%% @doc Gets the metadata of single package from the specified CKAN repository.
%% @spec package(RepositoryUrl, PackageId) -> {ok, PackageInfo} 
%%                                            | {error, Reason}
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------    
package(RepositoryUrl, PackageId) ->
    rest:get_json(resource_url(RepositoryUrl, ["package", PackageId])).
    
%%--------------------------------------------------------------------------
%% @doc Gets a list of id's of all the groups available at [http://ckan.net]
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------    
groups() ->
    groups(?DEFAULT_REPOSITORY).
        
%%--------------------------------------------------------------------------
%% @doc Gets a list of id's of all the groups available at a CKAN repository.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
groups(RepositoryUrl) ->
    rest:get_json(resource_url(RepositoryUrl, ["group"])).
    
%%--------------------------------------------------------------------------
%% @doc Gets info about the specified group from [http://ckan.net].
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------    
group(GroupId) ->
    group(?DEFAULT_REPOSITORY, GroupId).
        
%%--------------------------------------------------------------------------
%% @docGets info about the specified group from a CKAN repository.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
group(RepositoryUrl, GroupId) ->
    rest:get_json(resource_url(RepositoryUrl, ["group", GroupId])).
    
%%--------------------------------------------------------------------------
%% @doc Gets a list of all the tags available at [http://ckan.net]
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------    
tags() ->
    tags(?DEFAULT_REPOSITORY).

%%--------------------------------------------------------------------------
%% @doc Gets a list of all the tags available at a CKAN repository.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
tags(RepositoryUrl) ->
    rest:get_json(resource_url(RepositoryUrl, ["tag"])).
    
%%--------------------------------------------------------------------------
%% @doc Gets the list of id's of packages that are associated with a tag.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------    
packages_with_tag(Tag) ->
    packages_with_tag(?DEFAULT_REPOSITORY, Tag).

%%--------------------------------------------------------------------------
%% @docGets Gets the list of id's of packages that are associated with a tag.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
packages_with_tag(RepositoryUrl, Tag) ->
    rest:get_json(resource_url(RepositoryUrl, ["tag", Tag])).

%%--------------------------------------------------------------------------
%% @doc Gets a list of id's of revisions for a package.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------    
package_revisions(PackageId) ->
    package_revisions(?DEFAULT_REPOSITORY, PackageId).

%%--------------------------------------------------------------------------
%% @doc Gets a list of id's of revisions for a package.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
package_revisions(RepositoryUrl, PackageId) ->
    Url = resource_url(RepositoryUrl, ["package", PackageId, "revisions"]),
    rest:get_json(Url).

%%--------------------------------------------------------------------------
%% @doc Gets a list of id's of all the revisions available 
%%      at [http://ckan.net]
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------    
revisions() ->
    revisions(?DEFAULT_REPOSITORY).

%%--------------------------------------------------------------------------
%% @doc Gets a list of id's of all the revisions available at 
%%      a CKAN repository.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
revisions(RepositoryUrl) ->
    rest:get_json(resource_url(RepositoryUrl, ["revision"])).

%%--------------------------------------------------------------------------
%% @doc Gets a list of id's of all the revisions available 
%%      at [http://ckan.net]
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------    
revision(RevisionId) ->
    revision(?DEFAULT_REPOSITORY, RevisionId).

%%--------------------------------------------------------------------------
%% @doc Gets a list of id's of all the revisions available at 
%%      a CKAN repository.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
revision(RepositoryUrl, RevisionId) ->
    rest:get_json(resource_url(RepositoryUrl, ["revision", RevisionId])).
    
    
%%%==========================================================================
%%% Search API
%%%==========================================================================


    
%%%==========================================================================
%%% Internal functions
%%%==========================================================================

resource_url(RepositoryUrl, Args) ->
    string:join([RepositoryUrl, ?API_PATH | Args], "/").

