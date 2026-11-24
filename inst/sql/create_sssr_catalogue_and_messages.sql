-- ==============================================================================
-- File: inst/sql/create_sssr_catalogue_and_messages.sql
-- Repository: DFE-Digital/dauPortalTools
-- Purpose: Companion DDL script to inst/sql/create_sssr_schema.sql creating
--          the shared portal messaging, app directory, and data catalogue tables
--          in [Data_Insight_Team].[01_SSSR].
--
-- Prerequisites: Run inst/sql/create_sssr_schema.sql first (requires [01_SSSR].[users]).
--
-- Tables Created (in dependency order):
--   1. [01_SSSR].[portal_messages]
--   2. [01_SSSR].[app_list]
--   3. [01_SSSR].[app_maintainers]
--   4. [01_SSSR].[data_type]
--   5. [01_SSSR].[data_table]
--   6. [01_SSSR].[data_table_owners]
--   7. [01_SSSR].[app_data_links]
-- ==============================================================================

USE [Data_Insight_Team];
GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO

-- ==============================================================================
-- 1. CLEANUP PHASE (Child Tables First)
-- ==============================================================================
DROP TABLE IF EXISTS [01_SSSR].[app_data_links];
DROP TABLE IF EXISTS [01_SSSR].[data_table_owners];
DROP TABLE IF EXISTS [01_SSSR].[data_table];
DROP TABLE IF EXISTS [01_SSSR].[data_type];
DROP TABLE IF EXISTS [01_SSSR].[app_maintainers];
DROP TABLE IF EXISTS [01_SSSR].[app_list];
DROP TABLE IF EXISTS [01_SSSR].[portal_messages];
GO

-- ==============================================================================
-- 2. PORTAL MESSAGING & BANNERS
-- ==============================================================================
CREATE TABLE [01_SSSR].[portal_messages] (
    [message_id]   INT            IDENTITY(1,1) NOT NULL,
    [message_date] DATETIME2(0)   NOT NULL CONSTRAINT [DF_portal_messages_message_date] DEFAULT (SYSUTCDATETIME()),
    [message_text] NVARCHAR(4000) NULL,
    [app_id]       INT            NOT NULL,
    [priority]     TINYINT        NOT NULL CONSTRAINT [DF_portal_messages_priority] DEFAULT ((2)),
    [created_on]   DATETIME2(0)   NOT NULL CONSTRAINT [DF_portal_messages_created_on] DEFAULT (SYSUTCDATETIME()),
    [is_active]    BIT            NOT NULL CONSTRAINT [DF_portal_messages_is_active] DEFAULT ((1)),
    [user_id]      INT            NULL,

    CONSTRAINT [PK_portal_messages] PRIMARY KEY CLUSTERED ([message_id] ASC),
    CONSTRAINT [FK_portal_messages_user] FOREIGN KEY ([user_id])
        REFERENCES [01_SSSR].[users] ([user_id])
) ON [PRIMARY];
GO

-- ==============================================================================
-- 3. APPLICATION DIRECTORY & MAINTAINERS
-- ==============================================================================
CREATE TABLE [01_SSSR].[app_list] (
    [app_id]          INT            IDENTITY(1,1) NOT NULL,
    [app_name]        NVARCHAR(128)  NULL,
    [app_description] VARCHAR(250)   NULL,
    [app_url]         VARCHAR(2048)  NULL,
    [app_active]      BIT            NULL CONSTRAINT [DF_app_list_app_active] DEFAULT ((1)),
    [version]         NVARCHAR(50)   NULL CONSTRAINT [DF_app_list_version] DEFAULT ('v1.0.0'),
    [team]            NVARCHAR(150)  NULL,
    [update_cycle]    NVARCHAR(100)  NULL CONSTRAINT [DF_app_list_update_cycle] DEFAULT ('Monthly'),
    [created_date]    DATETIME2(0)   NOT NULL CONSTRAINT [DF_app_list_created_date] DEFAULT (SYSUTCDATETIME()),
    [created_by]      INT            NULL,
    [modified_date]   DATETIME2(0)   NULL,
    [modified_by]     INT            NULL,
    [reviewed_date]   DATE           NULL,
    [reviewed_by]     INT            NULL,
    [app_guidance]    NVARCHAR(1000) NULL,

    CONSTRAINT [PK_app_list] PRIMARY KEY CLUSTERED ([app_id] ASC)
) ON [PRIMARY];
GO

CREATE TABLE [01_SSSR].[app_maintainers] (
    [id]           INT          IDENTITY(1,1) NOT NULL,
    [app_id]       INT          NULL,
    [user_id]      INT          NULL,
    [is_primary]   BIT          NULL CONSTRAINT [DF_app_maintainers_is_primary] DEFAULT ((1)),
    [created_date] DATETIME2(0) NULL CONSTRAINT [DF_app_maintainers_created_date] DEFAULT (SYSUTCDATETIME()),
    [created_by]   INT          NULL,

    CONSTRAINT [PK_app_maintainers] PRIMARY KEY CLUSTERED ([id] ASC),
    CONSTRAINT [FK_app_maintainers_app_id] FOREIGN KEY ([app_id])
        REFERENCES [01_SSSR].[app_list] ([app_id]) ON DELETE CASCADE,
    CONSTRAINT [FK_app_maintainers_user_id] FOREIGN KEY ([user_id])
        REFERENCES [01_SSSR].[users] ([user_id])
) ON [PRIMARY];
GO

-- ==============================================================================
-- 4. DATA ASSET CATALOGUE & LINEAGE
-- ==============================================================================
CREATE TABLE [01_SSSR].[data_type] (
    [type_id]          INT           IDENTITY(1,1) NOT NULL,
    [type_name]        NVARCHAR(100) NOT NULL,
    [type_description] NVARCHAR(255) NULL,
    [is_active]        BIT           NOT NULL CONSTRAINT [DF_data_type_is_active] DEFAULT ((1)),
    [created_date]     DATETIME2(0)  NOT NULL CONSTRAINT [DF_data_type_created_date] DEFAULT (SYSUTCDATETIME()),
    [created_by]       INT           NULL,
    [modified_date]    DATETIME2(0)  NULL,
    [modified_by]      INT           NULL,
    [type_colour]      VARCHAR(20)   NOT NULL CONSTRAINT [DF_data_type_type_colour] DEFAULT ('#1d70b8'),

    CONSTRAINT [PK_data_type] PRIMARY KEY CLUSTERED ([type_id] ASC)
) ON [PRIMARY];
GO

CREATE TABLE [01_SSSR].[data_table] (
    [data_id]          INT           IDENTITY(1,1) NOT NULL,
    [data_type_fk]     INT           NOT NULL,
    [data_name]        NVARCHAR(150) NOT NULL,
    [data_table]       NVARCHAR(255) NOT NULL,
    [data_pk]          NVARCHAR(100) NULL,
    [platform]         VARCHAR(20)   NOT NULL CONSTRAINT [DF_data_table_platform] DEFAULT ('SQL'),
    [source_scope]     VARCHAR(20)   NOT NULL CONSTRAINT [DF_data_table_source_scope] DEFAULT ('DAU'),
    [data_description] NVARCHAR(500) NULL,
    [is_active]        BIT           NOT NULL CONSTRAINT [DF_data_table_is_active] DEFAULT ((1)),
    [created_date]     DATETIME2(0)  NOT NULL CONSTRAINT [DF_data_table_created_date] DEFAULT (SYSUTCDATETIME()),
    [created_by]       INT           NULL,
    [modified_date]    DATETIME2(0)  NULL,
    [modified_by]      INT           NULL,

    CONSTRAINT [PK_data_table] PRIMARY KEY CLUSTERED ([data_id] ASC),
    CONSTRAINT [FK_data_table_data_type] FOREIGN KEY ([data_type_fk])
        REFERENCES [01_SSSR].[data_type] ([type_id]),
    CONSTRAINT [CK_data_table_platform] CHECK ([platform] IN ('DataBricks', 'SQL')),
    CONSTRAINT [CK_data_table_source_scope] CHECK ([source_scope] IN ('External', 'Internal', 'DAU'))
) ON [PRIMARY];
GO

CREATE TABLE [01_SSSR].[data_table_owners] (
    [owner_link_id] INT          IDENTITY(1,1) NOT NULL,
    [data_id]       INT          NOT NULL,
    [user_id]       INT          NOT NULL,
    [is_primary]    BIT          NOT NULL CONSTRAINT [DF_data_table_owners_is_primary] DEFAULT ((0)),
    [created_date]  DATETIME2(0) NOT NULL CONSTRAINT [DF_data_table_owners_created_date] DEFAULT (SYSUTCDATETIME()),
    [created_by]    INT          NULL,
    [modified_date] DATETIME2(0) NULL,
    [modified_by]   INT          NULL,

    CONSTRAINT [PK_data_table_owners] PRIMARY KEY CLUSTERED ([owner_link_id] ASC),
    CONSTRAINT [FK_data_table_owners_data] FOREIGN KEY ([data_id])
        REFERENCES [01_SSSR].[data_table] ([data_id]) ON DELETE CASCADE,
    CONSTRAINT [FK_data_table_owners_user] FOREIGN KEY ([user_id])
        REFERENCES [01_SSSR].[users] ([user_id]) ON DELETE CASCADE
) ON [PRIMARY];
GO

CREATE TABLE [01_SSSR].[app_data_links] (
    [link_id]       INT          IDENTITY(1,1) NOT NULL,
    [app_id]        INT          NOT NULL,
    [data_id]       INT          NOT NULL,
    [access_mode]   VARCHAR(20)  NOT NULL CONSTRAINT [DF_app_data_links_access_mode] DEFAULT ('READ'),
    [is_active]     BIT          NOT NULL CONSTRAINT [DF_app_data_links_is_active] DEFAULT ((1)),
    [created_date]  DATETIME2(0) NOT NULL CONSTRAINT [DF_app_data_links_created_date] DEFAULT (SYSUTCDATETIME()),
    [created_by]    INT          NULL,
    [modified_date] DATETIME2(0) NULL,
    [modified_by]   INT          NULL,

    CONSTRAINT [PK_app_data_links] PRIMARY KEY CLUSTERED ([link_id] ASC),
    CONSTRAINT [FK_app_data_links_app] FOREIGN KEY ([app_id])
        REFERENCES [01_SSSR].[app_list] ([app_id]) ON DELETE CASCADE,
    CONSTRAINT [FK_app_data_links_data] FOREIGN KEY ([data_id])
        REFERENCES [01_SSSR].[data_table] ([data_id]) ON DELETE CASCADE,
    CONSTRAINT [CK_app_data_links_access_mode] CHECK ([access_mode] IN ('READ_WRITE', 'WRITE', 'READ'))
) ON [PRIMARY];
GO