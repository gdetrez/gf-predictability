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
    <xsl:if test="pos = 'av'">
      <xsl:value-of select="saldo" />
      <xsl:text>,</xsl:text>

      <xsl:choose>

	<xsl:when test="table/form/param = 'invar'">
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'invar']" />
	</xsl:when>
	
	<xsl:otherwise>
	  <xsl:value-of select="table/form/wf[../param = 'pos indef sg u nom']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'pos indef sg u gen']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'pos indef sg n nom']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'pos indef sg n gen']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'pos indef pl nom']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'pos indef pl gen']" />
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'pos def sg no_masc nom']"/>
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'pos def sg no_masc gen']"/>
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'pos def pl nom']"/>
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'pos def pl gen']"/>
	  <xsl:text>,</xsl:text>
	  
	  
	  <xsl:value-of select="table/form/wf[../param = 'komp nom']"/>
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'komp gen']"/>
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'super indef nom']"/>
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'super indef gen']"/>
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'super def no_masc nom']"/>
	  <xsl:text>,</xsl:text>
	  <xsl:value-of select="table/form/wf[../param = 'super def no_masc gen']"/>
	</xsl:otherwise>
      </xsl:choose>
      
      <xsl:text>&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
