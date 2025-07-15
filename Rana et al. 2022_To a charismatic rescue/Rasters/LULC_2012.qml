<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis maxScale="0" minScale="1e+08" hasScaleBasedVisibilityFlag="0" styleCategories="AllStyleCategories" version="3.16.0-Hannover">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <temporal mode="0" enabled="0" fetchMode="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <property key="WMSBackgroundLayer" value="false"/>
    <property key="WMSPublishDataSourceUrl" value="false"/>
    <property key="embeddedWidgets/count" value="0"/>
    <property key="identify/format" value="Value"/>
  </customproperties>
  <pipe>
    <provider>
      <resampling zoomedOutResamplingMethod="nearestNeighbour" zoomedInResamplingMethod="nearestNeighbour" enabled="false" maxOversampling="2"/>
    </provider>
    <rasterrenderer alphaBand="-1" nodataColor="" type="paletted" opacity="1" band="1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <colorPalette>
        <paletteEntry label="Plantations" color="#dc0f0f" value="10" alpha="255"/>
        <paletteEntry label="Deciduous Needleleaf Forest" color="#efb94d" value="16" alpha="255"/>
        <paletteEntry label="Aquaculture" color="#c882eb" value="11" alpha="255"/>
        <paletteEntry label="Mangrove Forest" color="#78ea3f" value="12" alpha="255"/>
        <paletteEntry label="Salt Pan" color="#2525ed" value="13" alpha="255"/>
        <paletteEntry label="Snow &amp; Ice" color="#e021a1" value="18" alpha="255"/>
        <paletteEntry label="Fallow Land" color="#33e6ab" value="7" alpha="255"/>
        <paletteEntry label="Wasteland" color="#7391cd" value="8" alpha="255"/>
        <paletteEntry label="Mixed Forest" color="#6f2fee" value="4" alpha="255"/>
        <paletteEntry label="Barren Land" color="#64ef64" value="6" alpha="255"/>
        <paletteEntry label="Permanent Wetlands" color="#45ca71" value="17" alpha="255"/>
        <paletteEntry label="Built-up Land" color="#a0ce44" value="3" alpha="255"/>
        <paletteEntry label="Evergreen Needleleaf Forest" color="#d376d3" value="19" alpha="255"/>
        <paletteEntry label="20" color="#de5713" value="20" alpha="255"/>
        <paletteEntry label="Cropland" color="#e1e16e" value="2" alpha="255"/>
        <paletteEntry label="Deciduous Broadleaf Forest" color="#c96284" value="1" alpha="255"/>
        <paletteEntry label="Shrubland" color="#3c9ecf" value="5" alpha="255"/>
        <paletteEntry label="Grassland" color="#3cc9c9" value="14" alpha="255"/>
      </colorPalette>
      <colorramp name="[source]" type="randomcolors"/>
    </rasterrenderer>
    <brightnesscontrast brightness="0" gamma="1" contrast="0"/>
    <huesaturation colorizeStrength="100" colorizeOn="0" colorizeRed="255" grayscaleMode="0" colorizeGreen="128" saturation="0" colorizeBlue="128"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
