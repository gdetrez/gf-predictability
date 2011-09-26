<?xml version="1.0" encoding="UTF-8"?><!-- -*-  mode: xml -*- -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"  
		version="1.0">

  <xsl:output method="text" encoding="utf-8"/>
  <xsl:strip-space elements="*" />
  
  <xsl:template match="/">
      <xsl:for-each select="Lexicon/LexicalEntry">
	<xsl:apply-templates select="." />
      </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="LexicalEntry">
    <xsl:if test="pos = 'nn'">
	<xsl:value-of select="saldo" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="table/form/wf[../param = 'sg indef nom']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="table/form/wf[../param = 'sg indef gen']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="table/form/wf[../param = 'sg def nom']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="table/form/wf[../param = 'sg def gen']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="table/form/wf[../param = 'pl indef nom']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="table/form/wf[../param = 'pl indef gen']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="table/form/wf[../param = 'pl def nom']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="table/form/wf[../param = 'pl def gen']" />
	<xsl:text>,</xsl:text>
	<xsl:choose>
	  <xsl:when test="inhs = 'u'">CommonScand.Utr</xsl:when>
	  <xsl:when test="inhs = 'n'">CommonScand.Neutr</xsl:when>
	  <xsl:otherwise>UNKNOWN</xsl:otherwise>
	</xsl:choose>
	<xsl:text>&#xA;</xsl:text>
	<xsl:value-of select="inhs" />
	<xsl:text>&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
