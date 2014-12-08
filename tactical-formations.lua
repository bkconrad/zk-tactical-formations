function widget:GetInfo()
	return {
		name      = "Tactical Formations",
		desc      = "Awesome tactical formations",
		author    = "kaen", -- Based on 'Custom Formations 2' by Niobium and Skasi
		version   = "v0.2",
		date      = "Dec, 2014",
		license   = "GNU GPL, v2 or later",
		layer     = 999999,
		enabled   = true,
		handler   = true,
	}
end

VFS.Include("LuaRules/Configs/customcmds.h.lua")

local MINIMUM_SPACE = 50

local ROLES = {
  RAIDER = {
    amphraider3 = true,
    armflea = true,
    armpt = true,
    armpw = true,
    corak = true,
    corclog = true, -- TODO
    corfav = true,
    corgator = true,
    corpyro = true,
    corsh = true,
    corsub = true,
    logkoda = true,
    panther = true,
    puppy = true,
  },

  RIOT = {
    amphriot = true,
    arm_venom = true,
    armwar = true,
    coresupp = true,
    corlevlr = true,
    cormak = true,
    dclship = true,
    hoverriot = true,
    spiderriot = true,
  },

  SKIRMISHER = {
    amphraider2 = true,
    armrock = true,
    armsptk = true,
    cormist = true,
    corroy = true,
    corstorm = true,
    nsaclash = true,
    slowmort = true,
    tawf114 = true,
  },

  CONSTRUCTOR = {
    amphcon = true,
    arm_spider = true,
    armrectr = true,
    coracv = true,
    corch = true,
    corfast = true,
    cornecro = true,
    corned = true,
    shipcon = true,
  },

  ARTILLERY = {
    armham = true,
    armmerl = true,
    armroy = true,
    corgarp = true,
    cormart = true,
    jumpblackhole = true,
    shieldarty = true,
    trem = true,
  },

  HEAVY = {
    amphassault = true,
    amphtele = true,
    armcrabe = true,
    armmanni = true,
    armsnipe = true,
    armspy = true,
    armtboat = true,
    capturecar = true,
    corcan = true,
    core_spectre = true,
    corgol = true,
    corsktl = true, -- TODO
    corsumo = true,
    firewalker = true,
    hoverdepthcharge = true,
    serpent = true,
    shieldfelon = true,
    spherecloaker = true,
    spherepole = true,
  },

  AA = {
    amphaa = true,
    armaak = true,
    armjeth = true,
    corarch = true,
    corcrash = true,
    correap = true,
    corsent = true,
    hoveraa = true,
    spideraa = true,
    vehaa = true,
  },

  ASSAULT = {
    amphfloater = true,
    armzeus = true,
    corraid = true,
    corthud = true,
    hoverassault = true,
    spiderassault = true,
  }
}

local ROLE_COLORS = {
  --               r    g    b    a
  ------------------------------------
  RAIDER      = { 0.0, 1.0, 0.0, 1.0 },
  RIOT        = { 1.0, 0.0, 0.0, 1.0 },
  HEAVY       = { 1.0, 1.0, 1.0, 1.0 },
  ARTILLERY   = { 1.0, 0.0, 1.0, 1.0 },
  CONSTRUCTOR = { 1.0, 1.0, 0.0, 1.0 },
  ASSAULT     = { 1.0, 1.0, 0.0, 1.0 },
  AA          = { 0.8, 0.8, 1.0, 1.0 },
  SKIRMISHER  = { 0.0, 0.0, 1.0, 1.0 }
}

local FORMATIONS = {
  default = {
    --               x1    y1    x2    y2
    ----------------------------------------
    ASSAULT     = { -1.0,  1.0,  1.0,  0.7 },
    RIOT        = { -1.0,  0.7,  1.0,  0.5 },
    RAIDER      = { -1.0,  0.5,  1.0,  0.2 },
    SKIRMISHER  = { -1.0,  0.2,  1.0,  0.0 },
    CONSTRUCTOR = { -1.0,  0.0,  1.0, -0.2 },
    ARTILLERY   = { -1.0, -0.2,  1.0, -0.3 },
    AA          = { -0.5, -0.3,  0.5, -0.5 },
    HEAVY       = { -0.5, -0.5,  0.5, -0.8 }
  }
}

local gDrawingFormation = false
local gRotatingFormation = false
local gFormationStartPosition = nil
local gFormationStopPosition = nil
local gScaleX = 0
local gScaleY = 0
local gTheta = 0

function issueFormation(unitIds, centerX, centerY, scaleX, scaleY, theta)
  local positionsByRole, unitsByRole = constructFormation(unitIds, centerX, centerY, scaleX, scaleY, theta)
  if not (positionsByRole and unitsByRole) then return false end

  for role, positions in pairs(positionsByRole) do
    local orders = GetOrdersHungarian(positions, unitsByRole[role], #unitsByRole[role], false)
    for _, order in pairs(orders) do
      local unitId, position = unpack(order)
      Spring.GiveOrderToUnit(unitId, CMD.MOVE, position, { nil, nil, nil })
    end
  end
end

function constructFormation(unitIds, centerX, centerY, scaleX, scaleY, theta)
  if type(unitIds) ~= 'table' or #unitIds < 1 then return false end

  local unitsByRole = groupUnitsByRole(unitIds)
  local positionsByRole = { }
  for role, roleUnitIds in pairs(unitsByRole) do
    local rectangle = FORMATIONS['default'][role]
    rectangle = {
      rectangle[1] * scaleX,
      rectangle[2] * scaleY,
      rectangle[3] * scaleX,
      rectangle[4] * scaleY
    }
    
    positionsByRole[role] = distributeWithinRectangle(roleUnitIds, rectangle)
    rotatePoints(positionsByRole[role], theta)
    translatePoints(positionsByRole[role], centerX, centerY)
  end

  return positionsByRole, unitsByRole
end

function rotatePoints(points, theta)
  for k,v in pairs(points) do
    points[k] = { v[1] * math.cos(theta) - v[3] * math.sin(theta), v[2], v[1] * math.sin(theta) + v[3] * math.cos(theta) }
  end
end

function translatePoints(points, dx, dz)
  for k,v in pairs(points) do
    points[k] = { v[1] + dx, v[2], v[3] + dz }
  end
end

function groupUnitsByRole(unitIds)
  local result = { }
  for role, _ in pairs(ROLES) do
    result[role] = { }
    for i = #unitIds, 1, -1 do -- backwards, so we can remove units in place
      local id = unitIds[i]
      local unitDefId = Spring.GetUnitDefID(id)
      local unitName = UnitDefs[unitDefId].name
      if ROLES[role][unitName] then
        table.insert(result[role], id) 
        table.remove(unitIds, i)
      end
    end
  end

  return result
end

function distributeWithinRectangle(unitIds, rectangle)
  local result = { }
  local x1, y1, x2, y2 = unpack(rectangle)
  local perRow = math.min(#unitIds, math.abs(x2 - x1) / MINIMUM_SPACE + 1)
  local spacing = math.abs(x2 - x1) / math.max(perRow - 1, 1)

  local row = 0
  local col = 0
  local offsetX, offsetY = calculateRowOffsets(#unitIds, perRow, #unitIds, spacing, math.abs(x2 - x1), math.abs(y2 - y1))
  for i, id in ipairs(unitIds) do
    local x = x1 + spacing * col
    local y = y1 + spacing * row

    table.insert(result, {x + offsetX, 0, y + offsetY})

    if col > perRow then
      col = 0
      row = row + 1
      offsetX, offsetY = calculateRowOffsets(#unitIds, perRow, #unitIds - i, spacing, math.abs(x2 - x1), math.abs(y2 - y1))
    else
      col = col + 1
    end
  end

  return result
end

function calculateRowOffsets(totalUnits, unitsPerRow, unitsLeft, spacing, width, height)
  local expectedWidth = (math.min(unitsPerRow, unitsLeft) - 1) * spacing
  local offsetWidth = (width - expectedWidth) / 2

  local rows = math.ceil(totalUnits / unitsPerRow)

  local expectedHeight = (rows - 1) * spacing
  local offsetHeight = (height - expectedHeight) / 2

  return offsetWidth, offsetHeight
end

--------------------------------------------------------------------------------
-- Epic Menu Options
--------------------------------------------------------------------------------

-- TODO: Set up some options :)
options_path = 'Settings/Interface/Command Visibility/Formations'
options_order = { 'drawmode', 'linewidth', 'dotsize' }
options = {
	drawmode = {
		name = 'Draw mode',
		-- desc is not supported here :(
		-- desc = 'Change the formation display. Formations are drawn by moving the mouse while the mouse button is pressed. Supported commands are Move, Fight, Patrol, Manual attacks, Jump and with the ALT key held down Attack, Set target and Unload.'
		-- (new) players might not even know about custom formations, so ultimately this should probably be displayed above these options
		type = 'radioButton',
		value = 'lines',
		items={
			{key='lines', name='Lines only', desc='Draw stippled lines along the drawn formation'},
			{key='dots', name='Dots only', desc='Draw dots at command locations'},
			{key='both', name='Draw both', desc='Draw lines and dots'},
		},
	},
	
	linewidth = {
		name = 'Width of lines',
		type = 'number',
		value = 2,
		min = 1, max = 2, step=1,
		-- For some reason drawing lines fails for numbers higher than 2. 
	},
	
	dotsize = {
		name = 'Size of dots',
		type = 'number',
		value = 1,
		min = 0.5, max = 2, step=0.1,
	},
}

--------------------------------------------------------------------------------
-- User Configurable Constants
--------------------------------------------------------------------------------
-- Minimum spacing between commands (Squared) when drawing a path for a single unit, must be >16*16 (Or orders overlap and cancel)
local minPathSpacingSq = 50 * 50

-- Minimum line length to cause formation move instead of single-click-style order
local minFormationLength = 20

-- How long should algorithms take. (~0.1 gives visible stutter, default: 0.05)
local maxHngTime = 0.05 -- Desired maximum time for hungarian algorithm
local maxNoXTime = 0.05 -- Strict maximum time for backup algorithm

local defaultHungarianUnits	= 20 -- Need a baseline to start from when no config data saved
local minHungarianUnits		= 10 -- If we kept reducing maxUnits it can get to a point where it can never increase, so we enforce minimums on the algorithms.
local unitIncreaseThresh	= 0.85 -- We only increase maxUnits if the units are great enough for time to be meaningful

-- Alpha loss per second after releasing mouse
local lineFadeRate = 2.0

-- What commands are eligible for custom formations
local formationCmds = {
	[CMD.MOVE] = true,
	[CMD.FIGHT] = true,
	[CMD.PATROL] = true,
	[CMD.UNLOAD_UNIT] = true,
	[CMD_JUMP] = true, -- jump
}

-- What commands require alt to be held (Must also appear in formationCmds)
local requiresAlt = {
}

-- Context-based default commands that can be overridden (i.e. guard when mouseover unit)
-- If the mouse remains on the same target for both Press/Release then the formation is ignored and original command is issued.
-- Normal logic will follow after override, i.e. must be a formationCmd to get formation, alt must be held if requiresAlt, etc.
local overrideCmds = {
}

-- What commands are issued at a position or unit/feature ID (Only used by GetUnitPosition)
local positionCmds = {
	[CMD.MOVE]=true,		[CMD.ATTACK]=true,		[CMD.RECLAIM]=true,		[CMD.RESTORE]=true,		[CMD.RESURRECT]=true,
	[CMD.PATROL]=true,		[CMD.CAPTURE]=true,		[CMD.FIGHT]=true, 		[CMD.MANUALFIRE]=true,		[CMD_JUMP]=true, -- jump
	[CMD.UNLOAD_UNIT]=true,	[CMD.UNLOAD_UNITS]=true,[CMD.LOAD_UNITS]=true,	[CMD.GUARD]=true,		[CMD.AREA_ATTACK] = true,
}

--------------------------------------------------------------------------------
-- Globals
--------------------------------------------------------------------------------
local maxHungarianUnits = defaultHungarianUnits -- Also set when loading config

local fNodes = {} -- Formation nodes, filled as we draw
local fDists = {} -- fDists[i] = distance from node 1 to node i
local totaldxy = 0 -- Measure of distance mouse has moved, used to unjag lines drawn in minimap
local lineLength = 0 -- Total length of the line

local dimmCmd = nil -- The dimming command (Used for color)
local dimmNodes = {} -- The current nodes of dimming line
local dimmAlpha = 0 -- The current alpha of dimming line

local pathCandidate = false -- True if we should start a path on mouse move
local draggingPath = false -- True if we are dragging a path for unit(s) to follow
local lastPathPos = nil -- The last point added to the path, used for min-distance check

local overriddenCmd = nil -- The command we ignored in favor of move
local overriddenTarget = nil -- The target (for params) we ignored

local usingCmd = nil -- The command to execute across the line
local usingRMB = false -- If the command is the default it uses right click, otherwise it is active and uses left click
local inMinimap = false -- Is the line being drawn in the minimap
local endShift = false -- True to reset command when shift is released

local MiniMapFullProxy = (Spring.GetConfigInt("MiniMapFullProxy", 0) == 1)

--------------------------------------------------------------------------------
-- Speedups
--------------------------------------------------------------------------------
local GL_LINE_STRIP = GL.LINE_STRIP
local glVertex = gl.Vertex
local glLineStipple = gl.LineStipple
local glLineWidth = gl.LineWidth
local glColor = gl.Color
local glBeginEnd = gl.BeginEnd
local glPushMatrix = gl.PushMatrix
local glPopMatrix = gl.PopMatrix
local glScale = gl.Scale
local glTranslate = gl.Translate
local glLoadIdentity = gl.LoadIdentity

local spGetActiveCommand = Spring.GetActiveCommand
local spSetActiveCommand = Spring.SetActiveCommand
local spGetDefaultCommand = Spring.GetDefaultCommand
local spFindUnitCmdDesc = Spring.FindUnitCmdDesc
local spGetModKeyState = Spring.GetModKeyState
local spGetInvertQueueKey = Spring.GetInvertQueueKey
local spIsAboveMiniMap = Spring.IsAboveMiniMap
local spGetSelectedUnitsCount = Spring.GetSelectedUnitsCount
local spGetSelectedUnits = Spring.GetSelectedUnits
local spGetUnitDefID = Spring.GetUnitDefID
local spGiveOrder = Spring.GiveOrder
local spGetUnitIsTransporting = Spring.GetUnitIsTransporting
local spGetCommandQueue = Spring.GetCommandQueue
local spGetUnitPosition = Spring.GetUnitPosition
local spTraceScreenRay = Spring.TraceScreenRay
local spGetGroundHeight = Spring.GetGroundHeight
local spGetFeaturePosition = Spring.GetFeaturePosition
local spGiveOrderToUnit = Spring.GiveOrderToUnit
local spGetUnitHeight = Spring.GetUnitHeight
local spGetCameraPosition = Spring.GetCameraPosition
local spGetViewGeometry = Spring.GetViewGeometry
local spTraceScreenRay = Spring.TraceScreenRay

local mapSizeX, mapSizeZ = Game.mapSizeX, Game.mapSizeZ
local maxUnits = Game.maxUnits

local osclock = os.clock
local tsort = table.sort
local floor = math.floor
local ceil = math.ceil
local sqrt = math.sqrt
local sin = math.sin
local cos = math.cos
local max = math.max
local huge = math.huge
local pi2 = 2*math.pi

local CMD_INSERT = CMD.INSERT
local CMD_MOVE = CMD.MOVE
local CMD_ATTACK = CMD.ATTACK
local CMD_UNLOADUNIT = CMD.UNLOAD_UNIT
local CMD_UNLOADUNITS = CMD.UNLOAD_UNITS
local CMD_SET_WANTED_MAX_SPEED = CMD.SET_WANTED_MAX_SPEED
local CMD_OPT_ALT = CMD.OPT_ALT
local CMD_OPT_CTRL = CMD.OPT_CTRL
local CMD_OPT_META = CMD.OPT_META
local CMD_OPT_SHIFT = CMD.OPT_SHIFT
local CMD_OPT_RIGHT = CMD.OPT_RIGHT

local keyShift = 304

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------
local function GetModKeys()
	local alt, ctrl, meta, shift = spGetModKeyState()
	return alt, ctrl, meta, shift
end
local function GetUnitFinalPosition(uID)
	
	local ux, uy, uz = spGetUnitPosition(uID)
	
	local cmds = spGetCommandQueue(uID, -1)
	for i = #cmds, 1, -1 do
		
		local cmd = cmds[i]
		if (cmd.id < 0) or positionCmds[cmd.id] then
			
			local params = cmd.params
			if #params >= 3 then
				return params[1], params[2], params[3]
			else
				if #params == 1 then
					
					local pID = params[1]
					local px, py, pz
					
					if pID > maxUnits then
						px, py, pz = spGetFeaturePosition(pID - maxUnits)
					else
						px, py, pz = spGetUnitPosition(pID)
					end
					
					if px then
						return px, py, pz
					end
				end
			end
		end
	end
	
	return ux, uy, uz
end
local function SetColor(cmdID, alpha)
	if     cmdID == CMD_MOVE       then glColor(0.5, 1.0, 0.5, alpha) -- Green
	elseif cmdID == CMD_ATTACK     then glColor(1.0, 0.2, 0.2, alpha) -- Red
	elseif cmdID == CMD_UNLOADUNIT then glColor(1.0, 1.0, 0.0, alpha) -- Yellow
	elseif cmdID == CMD_UNIT_SET_TARGET then glColor(1, 0.75, 0, alpha) -- Orange
	elseif cmdID == CMD_UNIT_SET_TARGET_CIRCLE then glColor(1, 0.75, 0, alpha) -- Orange
	else                                glColor(0.5, 0.5, 1.0, alpha) -- Blue
	end
end
local function CanUnitExecute(uID, cmdID)
	
	if cmdID == CMD_UNLOADUNIT then
		local transporting = spGetUnitIsTransporting(uID)
		return (transporting and #transporting > 0)
	end
	
	return (spFindUnitCmdDesc(uID, cmdID) ~= nil)
end
local function GetExecutingUnits(cmdID)
	local units = {}
	local selUnits = spGetSelectedUnits()
	for i = 1, #selUnits do
		local uID = selUnits[i]
		if CanUnitExecute(uID, cmdID) then
			units[#units + 1] = uID
		end
	end
	return units
end

local function AddFNode(pos)
	
	local px, pz = pos[1], pos[3]
	if px < 0 or pz < 0 or px > mapSizeX or pz > mapSizeZ then
		return false
	end
	
	local n = #fNodes
	if n == 0 then
		fNodes[1] = pos
		fDists[1] = 0
	else
		local prevNode = fNodes[n]
		local dx, dz = px - prevNode[1], pz - prevNode[3]
		local distSq = dx*dx + dz*dz
		if distSq == 0.0 then -- Don't add if duplicate
			return false
		end
		
		local dis = sqrt(distSq)
		
		fNodes[n + 1] = pos
		fDists[n + 1] = fDists[n] + dis
		lineLength = lineLength + dis
	end
	
	totaldxy = 0
	return true
end

local function HasWaterWeapon(UnitDefID)
	local haswaterweapon = false
	local numweapons = #(UnitDefs[UnitDefID]["weapons"])
	for j=1, numweapons do
		local weapondefid = UnitDefs[UnitDefID]["weapons"][j]["weaponDef"]
		local iswaterweapon = WeaponDefs[weapondefid]["waterWeapon"]
		if iswaterweapon then haswaterweapon=true end
	end	
	return haswaterweapon
end

local function GetInterpNodes(mUnits)
		
	local number = #mUnits
	local spacing = fDists[#fNodes] / (#mUnits - 1)

	local haswaterweapon = {}
	for i=1, number do
		local UnitDefID = spGetUnitDefID(mUnits[i])
		haswaterweapon[i] = HasWaterWeapon(UnitDefID)
	end
	--result of this and code below is that the height of the aimpoint for a unit [i] will be:
	--(a) on GetGroundHeight(units aimed position), if the unit has a waterweapon
	--(b) on whichever is highest out of water surface (=0) and GetGroundHeight(units aimed position), if the unit does not have water weapon. 
	--in BA this must match the behaviour of prevent_range_hax or commands will get modified.
	
	local interpNodes = {}
	
	local sPos = fNodes[1]
	local sX = sPos[1]
	local sZ = sPos[3]
	local sDist = 0
	
	local eIdx = 2
	local ePos = fNodes[2]
	local eX = ePos[1]
	local eZ = ePos[3]
	local eDist = fDists[2]
	
	local sY 
	if haswaterweapon[1] then 
		sY = spGetGroundHeight(sX, sZ) 
	else 
		sY = math.max(0,spGetGroundHeight(sX,sZ)) 
	end
	interpNodes[1] = {sX, sY, sZ}
	
	for n = 1, number - 2 do
		
		local reqDist = n * spacing
		while (reqDist > eDist) do
			
			sX = eX
			sZ = eZ
			sDist = eDist
			
			eIdx = eIdx + 1
			ePos = fNodes[eIdx]
			eX = ePos[1]
			eZ = ePos[3]
			eDist = fDists[eIdx]
		end
		
		local nFrac = (reqDist - sDist) / (eDist - sDist)
		local nX = sX * (1 - nFrac) + eX * nFrac
		local nZ = sZ * (1 - nFrac) + eZ * nFrac
		local nY 
		if haswaterweapon[number+1] then 
			nY = spGetGroundHeight(nX, nZ) 
		else 
			nY = math.max(0,spGetGroundHeight(nX, nZ)) 
		end
		interpNodes[n + 1] = {nX, nY, nZ}
	end
	
	ePos = fNodes[#fNodes]
	eX = ePos[1]
	eZ = ePos[3]
	local eY 
	if haswaterweapon[number] then  eY=spGetGroundHeight(eX, eZ) else eY=math.max(0,spGetGroundHeight(eX, eZ)) end
	interpNodes[number] = {eX, eY, eZ}
	
	--DEBUG for i=1,number do Spring.Echo(interpNodes[i]) end
	
	return interpNodes
end
local function GetCmdOpts(alt, ctrl, meta, shift, right)
	
	local opts = { alt=alt, ctrl=ctrl, meta=meta, shift=shift, right=right }
	local coded = 0
	
	if alt   then coded = coded + CMD_OPT_ALT   end
	if ctrl  then coded = coded + CMD_OPT_CTRL  end
	if meta  then coded = coded + CMD_OPT_META  end
	if shift then coded = coded + CMD_OPT_SHIFT end
	if right then coded = coded + CMD_OPT_RIGHT end
	
	opts.coded = coded
	return opts
end

local function GiveNotifyingOrder(cmdID, cmdParams, cmdOpts)
	if widgetHandler:CommandNotify(cmdID, cmdParams, cmdOpts) then
		return
	end
	spGiveOrder(cmdID, cmdParams, cmdOpts.coded)
end

local function GiveNonNotifyingOrder(cmdID, cmdParams, cmdOpts)
	spGiveOrder(cmdID, cmdParams, cmdOpts.coded)
end

local function GiveNotifyingOrderToUnit(uID, cmdID, cmdParams, cmdOpts)
	local widgets = widgetHandler.widgets
	for i=1, #widgets do
		local w = widgets[i]
		if w.UnitCommandNotify and w:UnitCommandNotify(uID, cmdID, cmdParams, cmdOpts) then
			return
		end
	end
	
	spGiveOrderToUnit(uID, cmdID, cmdParams, cmdOpts.coded)
end

--------------------------------------------------------------------------------
-- Mouse/keyboard Callins
--------------------------------------------------------------------------------
local p = Spring.Echo
function widget:MousePress(mx, my, mButton)
	
  lineLength = 0
  -- Where did we click
  inMinimap = spIsAboveMiniMap(mx, my)
  if inMinimap then return false end
  p('minimap')
  
  -- Get command that would've been issued
  local _, activeCmdID = spGetActiveCommand()
  if activeCmdID then
    if mButton ~= 1 then 
      return false 
    end
    
    usingCmd = activeCmdID
    usingRMB = false
  else
    if mButton ~= 3 then 
      return false 
    end
    
    local _, defaultCmdID = spGetDefaultCommand()
    usingCmd = defaultCmdID or CMD.MOVE
    usingRMB = true
  end
  p('activecmd')
  
  -- Without this, the unloads issued will use the area of the last area unload
  if usingCmd == CMD_UNLOADUNITS then
    usingCmd = CMD_UNLOADUNIT
  end
  
  -- Is this command eligible for a custom formation ?
  local alt, ctrl, meta, shift = GetModKeys()
  if not (formationCmds[usingCmd]) then
    return false
  end
  p('formationcmds')
  
  -- Get clicked position
  local _, pos = spTraceScreenRay(mx, my, true, inMinimap)
  if not pos then return false end
  p('position')

  if not (gDrawingFormation or gRotatingFormation) then
    if shift and ctrl then
      gTheta = 0
      gScaleX = 0
      gScaleY = 0
      gDrawingFormation = true
      gFormationStartPosition = pos
    else
      return false
    end
    p('started')
  end
  
  -- We handled the mouse press
  return true
end

function widget:MouseMove(mx, my, dx, dy, mButton)
	-- Minimap-specific checks
	if inMinimap then
		totaldxy = totaldxy + dx*dx + dy*dy
		if (totaldxy < 5) or not spIsAboveMiniMap(mx, my) then
			return false
		end
	end
  
  if gDrawingFormation or gRotatingFormation then
    widgetHandler:UpdateWidgetCallIn("DrawWorld", self)
  end
	
	-- Get clicked position
	local _, pos = spTraceScreenRay(mx, my, true, inMinimap)
	if not pos then return false end
  gFormationStopPosition = pos
	
	return false
end

function widget:MouseRelease(mx, my, mButton)

  local _, pos = spTraceScreenRay(mx, my, true, inMinimap)
  local result = false
  gFormationStopPosition = pos

  local dx = gFormationStopPosition[1] - gFormationStartPosition[1]
  local dz = gFormationStopPosition[3] - gFormationStartPosition[3]
  if gRotatingFormation then
    gRotatingFormation = false
    gTheta = math.atan2(dz, dx)

    -- This is where the magic happens
    result = issueFormation(Spring.GetSelectedUnits(), gFormationStartPosition[1], gFormationStartPosition[3], gScaleX, gScaleY, gTheta)

    gFormationStartPosition = nil
    gFormationStopPosition = nil

  elseif gDrawingFormation then

    gScaleX = math.abs(dx)
    gScaleY = math.abs(dz)
    gRotatingFormation = true
    gDrawingFormation = false

  end

  return result
end

function widget:KeyRelease(key)
	if (key == keyShift) and endShift then
		spSetActiveCommand(0)
		endShift = false
	end
end

--------------------------------------------------------------------------------
-- Drawing
--------------------------------------------------------------------------------

local function DrawFilledCircle(pos, size, cornerCount)
	glPushMatrix()
	glTranslate(pos[1], pos[2], pos[3])
	glBeginEnd(GL.TRIANGLE_FAN, function()
		glVertex(0,0,0)
		for t = 0, pi2, pi2 / cornerCount do
			glVertex(sin(t) * size, 0, cos(t) * size)
		end
	end)
	glPopMatrix()
end

function widget:DrawWorld()
  if not (gDrawingFormation or gRotatingFormation) then
	  widgetHandler:RemoveWidgetCallIn("DrawWorld", self)
    return false
  end

  local dx = gFormationStopPosition[1] - gFormationStartPosition[1]
  local dz = gFormationStopPosition[3] - gFormationStartPosition[3]
  if gDrawingFormation then
    gScaleX = math.abs(dx)
    gScaleY = math.abs(dz)
  elseif gRotatingFormation then
    gTheta = math.atan2(dz, dx)
  end

  local positionsByRole, _ = constructFormation(Spring.GetSelectedUnits(), gFormationStartPosition[1], gFormationStartPosition[3], gScaleX, gScaleY, gTheta)
  for role, positions in pairs(positionsByRole) do
    for _, position in pairs(positions) do
      glColor(0, 0, 0, 1.0)
      DrawFilledCircle(position, MINIMUM_SPACE / 2, 24)
      glColor(unpack(ROLE_COLORS[role]))
      DrawFilledCircle(position, MINIMUM_SPACE / 3, 24)
    end
  end
end

---------------------------------------------------------------------------------------------------------
-- Config
---------------------------------------------------------------------------------------------------------
function widget:GetConfigData() -- Saving
	return {
		['maxHungarianUnits'] = maxHungarianUnits,
	}
end
function widget:SetConfigData(data) -- Loading
	maxHungarianUnits = data['maxHungarianUnits'] or defaultHungarianUnits
end

---------------------------------------------------------------------------------------------------------
-- Matching Algorithms
---------------------------------------------------------------------------------------------------------
function GetOrdersNoX(nodes, units, unitCount, shifted)
	
	-- Remember when  we start
	-- This is for capping total time
	-- Note: We at least complete initial assignment
	local startTime = osclock()
	
	---------------------------------------------------------------------------------------------------------
	-- Find initial assignments
	---------------------------------------------------------------------------------------------------------
	local unitSet = {}
	local fdist = -1
	local fm
	
	for u = 1, unitCount do
		
		-- Get unit position
		local ux, uz
		if shifted then
			ux, _, uz = GetUnitFinalPosition(units[u])
		else
			ux, _, uz = spGetUnitPosition(units[u])
		end
		unitSet[u] = {ux, units[u], uz, -1} -- Such that x/z are in same place as in nodes (So we can use same sort function)
		
		-- Work on finding furthest points (As we have ux/uz already)
		for i = u - 1, 1, -1 do
			
			local up = unitSet[i]
			local vx, vz = up[1], up[3]
			local dx, dz = vx - ux, vz - uz
			local dist = dx*dx + dz*dz
			
			if (dist > fdist) then
				fdist = dist
				fm = (vz - uz) / (vx - ux)
			end
		end
	end
	
	-- Maybe nodes are further apart than the units
	for i = 1, unitCount - 1 do
		
		local np = nodes[i]
		local nx, nz = np[1], np[3]
		
		for j = i + 1, unitCount do
			
			local mp = nodes[j]
			local mx, mz = mp[1], mp[3]
			local dx, dz = mx - nx, mz - nz
			local dist = dx*dx + dz*dz
			
			if (dist > fdist) then
				fdist = dist
				fm = (mz - nz) / (mx - nx)
			end
		end
	end
	
	local function sortFunc(a, b)
		-- y = mx + c
		-- c = y - mx
		-- c = y + x / m (For perp line)
		return (a[3] + a[1] / fm) < (b[3] + b[1] / fm)
	end
	
	tsort(unitSet, sortFunc)
	tsort(nodes, sortFunc)
	
	for u = 1, unitCount do
		unitSet[u][4] = nodes[u]
	end
	
	---------------------------------------------------------------------------------------------------------
	-- Main part of algorithm
	---------------------------------------------------------------------------------------------------------
	
	-- M/C for each finished matching
	local Ms = {}
	local Cs = {}
	
	-- Stacks to hold finished and still-to-check units
	local stFin = {}
	local stFinCnt = 0
	local stChk = {}
	local stChkCnt = 0
	
	-- Add all units to check stack
	for u = 1, unitCount do
		stChk[u] = u
	end
	stChkCnt = unitCount
	
	-- Begin algorithm
	while ((stChkCnt > 0) and (osclock() - startTime < maxNoXTime)) do
		
		-- Get unit, extract position and matching node position
		local u = stChk[stChkCnt]
		local ud = unitSet[u]
		local ux, uz = ud[1], ud[3]
		local mn = ud[4]
		local nx, nz = mn[1], mn[3]
		
		-- Calculate M/C
		local Mu = (nz - uz) / (nx - ux)
		local Cu = uz - Mu * ux
		
		-- Check for clashes against finished matches
		local clashes = false
		
		for i = 1, stFinCnt do
			
			-- Get opposing unit and matching node position
			local f = stFin[i]
			local fd = unitSet[f]
			local tn = fd[4]
			
			-- Get collision point
			local ix = (Cs[f] - Cu) / (Mu - Ms[f])
			local iz = Mu * ix + Cu
			
			-- Check bounds
			if ((ux - ix) * (ix - nx) >= 0) and
			   ((uz - iz) * (iz - nz) >= 0) and
			   ((fd[1] - ix) * (ix - tn[1]) >= 0) and
			   ((fd[3] - iz) * (iz - tn[3]) >= 0) then
				
				-- Lines cross
				
				-- Swap matches, note this retains solution integrity
				ud[4] = tn
				fd[4] = mn
				
				-- Remove clashee from finished
				stFin[i] = stFin[stFinCnt]
				stFinCnt = stFinCnt - 1
				
				-- Add clashee to top of check stack
				stChkCnt = stChkCnt + 1
				stChk[stChkCnt] = f
				
				-- No need to check further
				clashes = true
				break
			end
		end
		
		if not clashes then
			
			-- Add checked unit to finished
			stFinCnt = stFinCnt + 1
			stFin[stFinCnt] = u
			
			-- Remove from to-check stack (Easily done, we know it was one on top)
			stChkCnt = stChkCnt - 1
			
			-- We can set the M/C now
			Ms[u] = Mu
			Cs[u] = Cu
		end
	end
	
	---------------------------------------------------------------------------------------------------------
	-- Return orders
	---------------------------------------------------------------------------------------------------------
	local orders = {}
	for i = 1, unitCount do
		local unit = unitSet[i]
		orders[i] = {unit[2], unit[4]}
	end
	return orders
end
function GetOrdersHungarian(nodes, units, unitCount, shifted)
	-------------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------
	-- (the following code is written by gunblob)
	--   this code finds the optimal solution (slow, but effective!)
	--   it uses the hungarian algorithm from http://www.public.iastate.edu/~ddoty/HungarianAlgorithm.html
	--   if this violates gpl license please let gunblob and me know
	-------------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------
	local t = osclock()
	
	--------------------------------------------------------------------------------------------
	--------------------------------------------------------------------------------------------
	-- cache node<->unit distances
	
	local distances = {}
	--for i = 1, unitCount do distances[i] = {} end
	
	for i = 1, unitCount do
		
		local uID = units[i]
		local ux, uz 
		
		if shifted then
			ux, _, uz = GetUnitFinalPosition(uID)
		else
			ux, _, uz = spGetUnitPosition(uID)
		end
		
		distances[i] = {}
		local dists = distances[i]
		for j = 1, unitCount do
			
			local nodePos = nodes[j]
			local dx, dz = nodePos[1] - ux, nodePos[3] - uz
			dists[j] = floor(sqrt(dx*dx + dz*dz) + 0.5)
			 -- Integer distances = greatly improved algorithm speed
		end
	end
	
	--------------------------------------------------------------------------------------------
	--------------------------------------------------------------------------------------------
	-- find optimal solution and send orders
	local result = findHungarian(distances, unitCount)
	
	--------------------------------------------------------------------------------------------
	--------------------------------------------------------------------------------------------
	-- determine needed time and optimize the maxUnits limit
	
	local delay = osclock() - t
	
	if (delay > maxHngTime) and (maxHungarianUnits > minHungarianUnits) then
		
		-- Delay is greater than desired, we have to reduce units
		maxHungarianUnits = maxHungarianUnits - 1
	else
		-- Delay is less than desired, so thats OK
		-- To make judgements we need number of units to be close to max
		-- Because we are making predictions of time and we want them to be accurate
		if (#units > maxHungarianUnits*unitIncreaseThresh) then
			
			-- This implementation of Hungarian algorithm is O(n3)
			-- Because we have less than maxUnits, but are altering maxUnits...
			-- We alter the time, to 'predict' time we would be getting at maxUnits
			-- We then recheck that against maxHngTime
			
			local nMult = maxHungarianUnits / #units
			
			if ((delay*nMult*nMult*nMult) < maxHngTime) then
				maxHungarianUnits = maxHungarianUnits + 1
			else
				if (maxHungarianUnits > minHungarianUnits) then
					maxHungarianUnits = maxHungarianUnits - 1
				end
			end
		end
	end
	
	-- Return orders
	local orders = {}
	for i = 1, unitCount do
		local rPair = result[i]
		orders[i] = {units[rPair[1]], nodes[rPair[2]]}
	end
	
	return orders
end

function findHungarian(array, n)
	
	-- Vars
	local colcover = {}
	local rowcover = {}
	local starscol = {}
	local primescol = {}
	
	-- Initialization
	for i = 1, n do
		rowcover[i] = false
		colcover[i] = false
		starscol[i] = false
		primescol[i] = false
	end
	
	-- Subtract minimum from rows
	for i = 1, n do
		
		local aRow = array[i]
		local minVal = aRow[1]
		for j = 2, n do
			if aRow[j] < minVal then
				minVal = aRow[j]
			end
		end
		
		for j = 1, n do
			aRow[j] = aRow[j] - minVal
		end
	end
	
	-- Subtract minimum from columns
	for j = 1, n do
		
		local minVal = array[1][j]
		for i = 2, n do
			if array[i][j] < minVal then
				minVal = array[i][j]
			end
		end
		
		for i = 1, n do
			array[i][j] = array[i][j] - minVal
		end
	end
	
	-- Star zeroes
	for i = 1, n do
		local aRow = array[i]
		for j = 1, n do
			if (aRow[j] == 0) and not colcover[j] then
				colcover[j] = true
				starscol[i] = j
				break
			end
		end
	end
	
	-- Start solving system
	while true do
		
		-- Are we done ?
		local done = true
		for i = 1, n do
			if not colcover[i] then
				done = false
				break
			end
		end
		
		if done then
			local pairings = {}
			for i = 1, n do
				pairings[i] = {i, starscol[i]}
			end
			return pairings
		end
		
		-- Not done
		local r, c = stepPrimeZeroes(array, colcover, rowcover, n, starscol, primescol)
		stepFiveStar(colcover, rowcover, r, c, n, starscol, primescol)
	end
end
function doPrime(array, colcover, rowcover, n, starscol, r, c, rmax, primescol)
	
	primescol[r] = c
	
	local starCol = starscol[r]
	if starCol then
		
		rowcover[r] = true
		colcover[starCol] = false
		
		for i = 1, rmax do
			if not rowcover[i] and (array[i][starCol] == 0) then
				local rr, cc = doPrime(array, colcover, rowcover, n, starscol, i, starCol, rmax, primescol)
				if rr then
					return rr, cc
				end
			end
		end
		
		return
	else
		return r, c
	end
end
function stepPrimeZeroes(array, colcover, rowcover, n, starscol, primescol)
	
	-- Infinite loop
	while true do
		
		-- Find uncovered zeros and prime them
		for i = 1, n do
			if not rowcover[i] then
				local aRow = array[i]
				for j = 1, n do
					if (aRow[j] == 0) and not colcover[j] then
						local i, j = doPrime(array, colcover, rowcover, n, starscol, i, j, i-1, primescol)
						if i then
							return i, j
						end
						break -- this row is covered
					end
				end
			end
		end
		
		-- Find minimum uncovered
		local minVal = huge
		for i = 1, n do
			if not rowcover[i] then
				local aRow = array[i]
				for j = 1, n do
					if (aRow[j] < minVal) and not colcover[j] then
						minVal = aRow[j]
					end
				end
			end
		end
		
		-- There is the potential for minVal to be 0, very very rarely though. (Checking for it costs more than the +/- 0's)
		
		-- Covered rows = +
		-- Uncovered cols = -
		for i = 1, n do
			local aRow = array[i]
			if rowcover[i] then
				for j = 1, n do
					if colcover[j] then
						aRow[j] = aRow[j] + minVal
					end
				end
			else
				for j = 1, n do
					if not colcover[j] then
						aRow[j] = aRow[j] - minVal
					end
				end
			end
		end
	end
end
function stepFiveStar(colcover, rowcover, row, col, n, starscol, primescol)
	
	-- Star the initial prime
	primescol[row] = false
	starscol[row] = col
	local ignoreRow = row -- Ignore the star on this row when looking for next
	
	repeat
		local noFind = true
		
		for i = 1, n do
			
			if (starscol[i] == col) and (i ~= ignoreRow) then
				
				noFind = false
				
				-- Unstar the star
				-- Turn the prime on the same row into a star (And ignore this row (aka star) when searching for next star)
				
				local pcol = primescol[i]
				primescol[i] = false
				starscol[i] = pcol
				ignoreRow = i
				col = pcol
				
				break
			end
		end
	until noFind
	
	for i = 1, n do
		rowcover[i] = false
		colcover[i] = false
		primescol[i] = false
	end
	
	for i = 1, n do
		local scol = starscol[i]
		if scol then
			colcover[scol] = true
		end
	end
end
