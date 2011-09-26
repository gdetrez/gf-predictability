<?xml version="1.0" encoding="UTF-8"?><!-- -*-  mode: xml -*- -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"  
		version="1.0">

  <xsl:output method="text" encoding="utf-8"/>
  <xsl:strip-space elements="*" />
  
  <xsl:template match="/">
      <xsl:for-each select="lexicon/lexicalEntry">
	<xsl:apply-templates select="formSet"/>
      </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="formSet">
    <xsl:if test="lemmatizedForm/grammaticalCategory = 'adjective'">
	<xsl:value-of select="lemmatizedForm/orthography" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalNumber = 'singular' and ../grammaticalGender = 'masculine']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalNumber = 'plural' and ../grammaticalGender = 'masculine']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalNumber = 'singular' and ../grammaticalGender = 'feminine']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalNumber = 'plural' and ../grammaticalGender = 'feminine']" />
	<xsl:text>&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
