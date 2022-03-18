***ATTRIB Brand Share of Program Budget***



SELECT p.[Id] AS [ProgramId], p.[Name] AS [Program Name], p.[ClientId],
s.[Id] AS [SubBudgetId], s.[Total] AS [Current Program Amount to Use], s.[BrandId],
b.[Name] AS [Brand],
ttl.[TOTAL Current Program Cost],
(s.[Total] / ttl.[TOTAL Current Program Cost]) AS [Brand Share of Program Budget]
FROM [Program] p
LEFT JOIN [SubBudget] s on s.[ProgramId]=p.[Id]
LEFT JOIN [Brand] b ON b.[Id] = s.[BrandId]
LEFT JOIN
(SELECT SUM(s.[Total]) AS [TOTAL Current Program Cost], s.[ProgramId]
FROM [SubBudget] s
GROUP BY s.[ProgramId]) ttl ON ttl.[ProgramId]=p.[Id]
WHERE p.[ClientId] IN (6,8)


======================================================================


***ATTRIB MASTER PULL***


SELECT p.[ClientId], p.[Id] AS [Program Id], p.[Name] AS [Program Name],
t.[Id] AS [TacticId], t.[Name] AS [Tactic], t.[Category] AS [Tactic Category], t.[CategoryId], t.[DateStart] AS [Tactic Start Date], t.[DateEnd] AS [Tactic End Date], t.[CostInsertion], t.[CostRedemption], t.[Spend] As [Total Tactic Spend], t.[StoreCount], t.[RMN],
(p.[Id] + 0000 + t.[Id]) AS [Concat Id],
t.[VendorId], v.[Name] AS [Vendor],
b.[Name] AS [Brand], b.[Id] AS [BrandId],
bs.[Brand Share of Program Budget], (bs.[Brand Share of Program Budget] * t.[Spend]) AS [Brand Share of Total Tactic Spend],
(bs.[Brand Share of Program Budget] * t.[CostInsertion]) AS [Brand Share of Tactic Insertion Cost],
(bs.[Brand Share of Program Budget] * t.[CostRedemption]) AS [Brand Share of Tactic Redemption Cost],
sw.[Nielsen_Week_Year]
FROM [Program] p
LEFT JOIN [Tactic] t ON t.[ProgramId]=p.[Id]
LEFT JOIN [Vendor] v ON v.[Id]=t.[VendorId]
LEFT JOIN [Matrix_ProgramBrand] pb ON pb.[ProgramId]
LEFT JOIN [Brand] b ON pb.[BrandId] = b.[Id]
LEFT JOIN [ATTRIB Brand Share of Program Budget] bs ON bs.[ProgramId]=p.[Id] AND bs.[BrandId]=b.[Id]
LEFT JOIN [ATTRIB Syndicated Weeks by Tactic] sw ON sw.[TacticId]=t.[Id]

===========================



****ATTRIB Syndicated Weeks by Tactic****



SELECT DISTINCT
(Nielsen_Week_Year), week_temp.[Id] AS [TacticId]
FROM (
    SELECT
        MIN(dd.[Nielsen_Week_Year]) AS start_week,
        MAX(dd.[Nielsen_Week_Year]) AS end_week,
        t.[id]
    FROM  [Dim Date] dd
        INNER JOIN [Tactic] t ON dd.[Date] BETWEEN t.[DateStart] AND t.[DateEnd]
    GROUP BY
        t.[Id]
        ) week_temp
INNER JOIN [Dim Date] dd ON
    dd.[Nielsen_Week_Year] BETWEEN week_temp.[start_week] AND week_temp.[end_week]


================================



****ATTRIB Week Count By Tactic****



SELECT sw.[TacticId], COUNT(sw.[Nielsen_Week_Year])
FROM [ATTRIB Syndicated Weeks by Tactic] sw
GROUP BY sw.[TacticId]
(
SELECT distinct
    dd.[Nielsen Week] AS nielsen_week_number,
    week_temp.[Id],
    Nielsen_Week_Year,
    'Program' as type
FROM (
    SELECT
        MIN(dd.[Nielsen_Week_Year]) AS start_week,
        MAX(dd.[Nielsen_Week_Year]) AS end_week,
        t.[id]
    FROM  [Dim Date] dd
        INNER JOIN [Tactic] t ON dd.[Date] BETWEEN t.[DateStart] AND t.[DateEnd]
    GROUP BY
        t.[Id]
        ) week_temp
INNER JOIN [Dim Date] dd ON
    dd.[Nielsen_Week_Year] BETWEEN week_temp.[start_week] AND week_temp.[end_week]
)
week ON week.[Id]=t.[Id]




***** Need to left join to week_temp
----- Need to join tactic metrics 3 and 12 to get impressions and engagements
