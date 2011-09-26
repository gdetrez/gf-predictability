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
    <xsl:if test="lemmatizedForm/grammaticalCategory = 'verb'">
	<xsl:value-of select="lemmatizedForm/orthography" />
	<xsl:text>,</xsl:text>
	<!-- INFINITIVE -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'infinitive']" />
	<xsl:text>,</xsl:text>

	<!-- INDICATIVE PRESENT -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>

	<!-- SUBJUNTIVE PRESENT -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>


	<!-- INDICATIVE IMPERFECT -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>

	<!-- SUBJUNTIVE IMPERFECT -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'subjunctive' and ../grammaticalTense = 'imperfect' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>

	<!-- SIMPLE PAST -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'simplePast' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'simplePast' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'simplePast' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'simplePast' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'simplePast' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'simplePast' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>

	<!-- FUTURE -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'future' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'future' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'future' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'future' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'future' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'indicative' and ../grammaticalTense = 'future' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>

	<!-- CONDITIONAL PRESENT -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'conditional' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'conditional' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'conditional' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'conditional' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'conditional' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'conditional' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'thirdPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>

	<!-- IMPERATIVE -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'imperative' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'imperative' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'firstPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'imperative' and ../grammaticalTense = 'present' and ../grammaticalPerson = 'secondPerson' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>

	<!-- PAST PARTICIPLE -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'participle' and ../grammaticalTense = 'past' and ../grammaticalGender = 'masculine' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'participle' and ../grammaticalTense = 'past' and ../grammaticalGender = 'masculine' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'participle' and ../grammaticalTense = 'past' and ../grammaticalGender = 'feminine' and ../grammaticalNumber='singular']" />
	<xsl:text>,</xsl:text>
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'participle' and ../grammaticalTense = 'past' and ../grammaticalGender = 'feminine' and ../grammaticalNumber='plural']" />
	<xsl:text>,</xsl:text>
	<!-- PRESENT PARTICIPLE -->
	<xsl:value-of select="inflectedForm/orthography[../grammaticalMood = 'participle' and ../grammaticalTense = 'present']" />

	<xsl:text>&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
