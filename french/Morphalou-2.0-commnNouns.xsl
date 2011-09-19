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
    <xsl:if test="lemmatizedForm/grammaticalCategory = 'commonNoun'">
	<xsl:value-of select="lemmatizedForm/orthography" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalNumber = 'singular']" />
	<xsl:text>,</xsl:text>
	
	<xsl:value-of select="inflectedForm/orthography[../grammaticalNumber = 'plural']" />
	<xsl:text>,</xsl:text>
	<xsl:choose>
	  <xsl:when test="lemmatizedForm/grammaticalGender = 'masculine'">CommonRomance.Masc</xsl:when>
	  <xsl:when test="lemmatizedForm/grammaticalGender = 'feminine'">CommonRomance.Fem</xsl:when>
	  <xsl:otherwise>UNKNOWN</xsl:otherwise>
	</xsl:choose>
	<xsl:text>&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
