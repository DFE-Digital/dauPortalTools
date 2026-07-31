USE [Data_Insight_Team];
GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO

-- =================================================================================
-- 1. CLEANUP PHASE: DROP EXISTING TABLES (Child Tables First)
-- =================================================================================
PRINT 'Starting database cleanup...';

DROP TABLE IF EXISTS [01_SSSR].[audit_logs];
DROP TABLE IF EXISTS [01_SSSR].[app_analytics];
DROP TABLE IF EXISTS [01_SSSR].[role_permissions];
DROP TABLE IF EXISTS [01_SSSR].[permissions_config];
DROP TABLE IF EXISTS [01_SSSR].[user_role_assignments];
DROP TABLE IF EXISTS [01_SSSR].[user_identity_aliases];
DROP TABLE IF EXISTS [01_SSSR].[users];
DROP TABLE IF EXISTS [01_SSSR].[apps_config];
DROP TABLE IF EXISTS [01_SSSR].[roles_config];
DROP TABLE IF EXISTS [01_SSSR].[environments_config];

PRINT 'All target SSSR tables successfully dropped.';
GO


-- =================================================================================
-- 2. LOOKUP / CONFIGURATION TABLES (No Foreign Keys)
-- =================================================================================
PRINT 'Creating lookup and configuration tables...';

-- A. ENVIRONMENTS CONFIGURATION
CREATE TABLE [01_SSSR].[environments_config](
    [env_id] [tinyint] IDENTITY(1,1) NOT NULL,
    [env_name] [nvarchar](20) NOT NULL,                                 -- 'dev', 'test', 'prod'
    [friendly_name] [nvarchar](50) NOT NULL,                           -- 'Local Development', 'Production'
    
    CONSTRAINT [PK_environments_config] PRIMARY KEY CLUSTERED ([env_id] ASC),
    CONSTRAINT [UQ_environments_config_name] UNIQUE NONCLUSTERED ([env_name] ASC)
) ON [PRIMARY];

-- B. ROLES CONFIGURATION
CREATE TABLE [01_SSSR].[roles_config](
    [role_id] [int] IDENTITY(1,1) NOT NULL,
    [role_name] [nvarchar](50) NOT NULL,                                 -- 'admin', 'regional_admin'
    [friendly_name] [nvarchar](100) NOT NULL,                           -- 'Admin', 'Regional Admin'
    [description] [nvarchar](255) NULL,
    
    CONSTRAINT [PK_roles_config_id] PRIMARY KEY CLUSTERED ([role_id] ASC),
    CONSTRAINT [UQ_roles_config_name] UNIQUE NONCLUSTERED ([role_name] ASC)
) ON [PRIMARY];

-- C. PERMISSIONS CONFIGURATION (Dynamic Feature Flags)
CREATE TABLE [01_SSSR].[permissions_config](
    [permission_id] [int] IDENTITY(1,1) NOT NULL,
    [permission_name] [nvarchar](75) NOT NULL,                           -- 'can_delete_records', 'can_view_pii'
    [friendly_name] [nvarchar](100) NOT NULL,
    [description] [nvarchar](255) NULL,
    
    CONSTRAINT [PK_permissions_config] PRIMARY KEY CLUSTERED ([permission_id] ASC),
    CONSTRAINT [UQ_permissions_config_name] UNIQUE NONCLUSTERED ([permission_name] ASC)
) ON [PRIMARY];

-- D. APPLICATIONS CONFIGURATION
CREATE TABLE [01_SSSR].[apps_config](
    [app_id] [int] IDENTITY(1,1) NOT NULL,
    [app_name] [nvarchar](128) NOT NULL,
    [app_description] [nvarchar](500) NULL,
    [app_url] [nvarchar](2048) NULL,
    [linking_table] [nvarchar](128) NULL,
    [linking_column] [nvarchar](128) NULL,
    [is_active] [bit] NOT NULL CONSTRAINT [DF_apps_config_is_active] DEFAULT (1),
    
    CONSTRAINT [PK_apps_config] PRIMARY KEY CLUSTERED ([app_id] ASC)
) ON [PRIMARY];
GO


-- =================================================================================
-- 3. CORE IDENTITY TABLES
-- =================================================================================
PRINT 'Creating identity tables...';

-- E. MASTER USERS REGISTER
CREATE TABLE [01_SSSR].[users](
    [user_id] [int] IDENTITY(1,1) NOT NULL,
    [username] [nvarchar](100) NULL,                                    -- Nullable for email-first registration
    [email] [nvarchar](255) NOT NULL,                                   -- Strict corporate identity anchor
    [created_at] [datetime2](7) NOT NULL CONSTRAINT [DF_users_created_at] DEFAULT (SYSUTCDATETIME()),

    CONSTRAINT [PK_users_id] PRIMARY KEY CLUSTERED ([user_id] ASC),
    CONSTRAINT [UQ_users_email] UNIQUE NONCLUSTERED ([email] ASC)
) ON [PRIMARY];

-- Unique index to keep non-NULL usernames unique
CREATE UNIQUE NONCLUSTERED INDEX [UIX_users_username_filtered]
ON [01_SSSR].[users]([username])
WHERE [username] IS NOT NULL;

-- F. USER IDENTITY ALIASES
CREATE TABLE [01_SSSR].[user_identity_aliases](
    [username_alias] [nvarchar](100) NOT NULL,                          -- 'bsmith7' or full UPN email
    [email_address] [nvarchar](255) NOT NULL,                           -- Maps directly to SSSR.users email
    [created_at] [datetime2](7) NOT NULL CONSTRAINT [DF_user_identity_aliases_created_at] DEFAULT (SYSUTCDATETIME()),

    CONSTRAINT [PK_user_identity_aliases] PRIMARY KEY CLUSTERED ([username_alias] ASC)
) ON [PRIMARY];
GO


-- =================================================================================
-- 4. RELATION/JUNCTION TABLES (With Soft-Deletes & Referential Integrity)
-- =================================================================================
PRINT 'Creating relationship and junction tables...';

-- G. ROLE PERMISSIONS (Maps permissions directly to roles)
CREATE TABLE [01_SSSR].[role_permissions](
    [role_id] [int] NOT NULL,
    [permission_id] [int] NOT NULL,
    
    CONSTRAINT [PK_role_permissions] PRIMARY KEY CLUSTERED ([role_id] ASC, [permission_id] ASC),
    CONSTRAINT [FK_role_permissions_role] FOREIGN KEY ([role_id]) REFERENCES [01_SSSR].[roles_config] ([role_id]) ON DELETE CASCADE,
    CONSTRAINT [FK_role_permissions_permission] FOREIGN KEY ([permission_id]) REFERENCES [01_SSSR].[permissions_config] ([permission_id]) ON DELETE CASCADE
) ON [PRIMARY];

-- H. USER ROLE ASSIGNMENTS (With Soft-Delete Auditing)
CREATE TABLE [01_SSSR].[user_role_assignments](
    [assignment_id] [int] IDENTITY(1,1) NOT NULL,
    [user_id] [int] NOT NULL,
    [role_id] [int] NOT NULL,
    [app_id] [int] NOT NULL,
    [is_active] [bit] NOT NULL CONSTRAINT [DF_user_role_assignments_is_active] DEFAULT (1), -- Soft delete flag
    [assigned_at] [datetime2](7) NOT NULL CONSTRAINT [DF_user_role_assignments_assigned_at] DEFAULT (SYSUTCDATETIME()),
    [assigned_by] [int] NOT NULL,
    [revoked_at] [datetime2](7) NULL,
    [revoked_by] [int] NULL,

    CONSTRAINT [PK_user_role_assignments] PRIMARY KEY CLUSTERED ([assignment_id] ASC),
    CONSTRAINT [UQ_user_role_app_assignment] UNIQUE NONCLUSTERED ([user_id] ASC, [role_id] ASC, [app_id] ASC),
    
    CONSTRAINT [FK_user_role_assignments_user_id] FOREIGN KEY ([user_id]) REFERENCES [01_SSSR].[users] ([user_id]),
    CONSTRAINT [FK_user_role_assignments_role_id] FOREIGN KEY ([role_id]) REFERENCES [01_SSSR].[roles_config] ([role_id]),
    CONSTRAINT [FK_user_role_assignments_assigned_by] FOREIGN KEY ([assigned_by]) REFERENCES [01_SSSR].[users] ([user_id]),
    CONSTRAINT [FK_user_role_assignments_revoked_by] FOREIGN KEY ([revoked_by]) REFERENCES [01_SSSR].[users] ([user_id])
) ON [PRIMARY];
GO


-- =================================================================================
-- 5. TRANSACTIONAL & LOGGING TABLES (With Environment Segmentation)
-- =================================================================================
PRINT 'Creating telemetry and audit tables...';

-- I. APPLICATION PERFORMANCE & EVENT TELEMETRY
CREATE TABLE [01_SSSR].[app_analytics](
    [analytics_id] [int] IDENTITY(1,1) NOT NULL,
    [user_id] [int] NOT NULL,
    [app_id] [int] NOT NULL,
    [env_id] [tinyint] NOT NULL,                                         -- Segments Local Dev vs. Prod data!
    [event_timestamp] [datetime2](7) NOT NULL CONSTRAINT [DF_app_analytics_event_timestamp] DEFAULT (SYSUTCDATETIME()),
    [page_name] [nvarchar](150) NOT NULL,
    [action_type] [nvarchar](50) NOT NULL,                              -- 'Load', 'Click', 'Download'
    [action_sub_type] [nvarchar](255) NULL,

    CONSTRAINT [PK_app_analytics] PRIMARY KEY CLUSTERED ([analytics_id] ASC),
    CONSTRAINT [FK_app_analytics_users] FOREIGN KEY ([user_id]) REFERENCES [01_SSSR].[users] ([user_id]),
    CONSTRAINT [FK_app_analytics_apps] FOREIGN KEY ([app_id]) REFERENCES [01_SSSR].[apps_config] ([app_id]),
    CONSTRAINT [FK_app_analytics_envs] FOREIGN KEY ([env_id]) REFERENCES [01_SSSR].[environments_config] ([env_id])
) ON [PRIMARY];

-- J. UNIFIED PLATFORM AUDIT LOG
CREATE TABLE [01_SSSR].[audit_logs](
    [audit_id] [bigint] IDENTITY(1,1) NOT NULL,
    [app_id] [int] NOT NULL,                                            -- Switched to INT FK for relational normalization
    [user_id] [int] NOT NULL,
    [env_id] [tinyint] NOT NULL,                                         -- Segments Dev audits from Prod audits
    [action_type] [nvarchar](50) NOT NULL,                              -- 'INSERT', 'UPDATE', 'DELETE'
    [target_table] [nvarchar](100) NOT NULL,
    [record_id] [nvarchar](50) NULL,
    [action_summary] [nvarchar](max) NOT NULL,
    [created_date] [datetime2](7) NOT NULL CONSTRAINT [DF_audit_logs_created_date] DEFAULT (SYSUTCDATETIME()),

    CONSTRAINT [PK_audit_logs] PRIMARY KEY CLUSTERED ([audit_id] ASC),
    CONSTRAINT [FK_audit_logs_user_id] FOREIGN KEY ([user_id]) REFERENCES [01_SSSR].[users] ([user_id]),
    CONSTRAINT [FK_audit_logs_apps] FOREIGN KEY ([app_id]) REFERENCES [01_SSSR].[apps_config] ([app_id]),
    CONSTRAINT [FK_audit_logs_envs] FOREIGN KEY ([env_id]) REFERENCES [01_SSSR].[environments_config] ([env_id])
) ON [PRIMARY];
GO


-- =================================================================================
-- 6. SEED DATA GENERATION
-- =================================================================================
PRINT 'Pre-populating core reference data...';

-- Seed Environments
INSERT INTO [01_SSSR].[environments_config] ([env_name], [friendly_name])
VALUES ('dev', 'Development'), ('test', 'Testing/QA'), ('prod', 'Production');

-- Seed Roles
SET IDENTITY_INSERT [01_SSSR].[roles_config] ON;
INSERT INTO [01_SSSR].[roles_config] ([role_id], [role_name], [friendly_name], [description])
VALUES 
(1, 'admin', 'Admin', 'System-wide administrator with full access'),
(2, 'regional_admin', 'Regional Admin', 'Administrator for regional operations');
SET IDENTITY_INSERT [01_SSSR].[roles_config] OFF;

-- Seed Apps Directory
SET IDENTITY_INSERT [01_SSSR].[apps_config] ON;
INSERT INTO [01_SSSR].[apps_config] ([app_id], [app_name], [linking_column], [linking_table], [app_description], [app_url], [is_active])
VALUES 
(1,  N'Warning Notices portal', N'twn_id', N'[01_AIDT].[twn_all_notices]', NULL, N'https://rsconnect/rsc/warning-notice-portal/', 1),
(2,  N'Warning Notices Online Notices', N'post_twn_id', N'[01_AIDT].[twn_post_notices]', NULL, NULL, 0),
(3,  N'Significant Change Tracker', N'sig_change_id', N'[01_sigchange].[tracker]', NULL, N'https://rsconnect/rsc/sig-change-portal/', 1),
(4,  N'Portal Tools Test', NULL, NULL, NULL, NULL, 0),
(5,  N'ATsAT-User-Portal', N'na', N'na', N'Defunct Strategic Conversation PowerApp', NULL, 0),
(6,  N'Recording Significant Change', N'na', N'na', N'Defunct Significant Change PowerApp', NULL, 0),
(7,  N'TWNkl-User-Portal', N'na', N'na', N'Defunct Warning Notice PowerApp', NULL, 0),
(8,  N'TWNkl-PBi', N'na', N'na', N'Defunct Warning Notice PowerBI', NULL, 0),
(9,  N'SLIC (School Level Interventions and Changes)', NULL, NULL, N'the hub to SSSR tools and reporting', N'https://rsconnect/rsc/slic/', 1),
(10, N'rise-flow-identification', NULL, NULL, NULL, NULL, 0),
(11, N'RISE Universal Hubs Portal', NULL, NULL, N'RISE Universal Hubs Portal', NULL, 1),
(13, N'dauPortalTools', N'na', N'na', NULL, NULL, 0);
SET IDENTITY_INSERT [01_SSSR].[apps_config] OFF;
GO

PRINT 'Database setup completed successfully! All tables ready under the [01_SSSR] schema.';
GO