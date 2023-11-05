package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.util.io.FileUtil;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy.Concept;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.util.List;


public class TinkarStarterData {

    public static void main(String[] args){
        File datastore = new File(args[0]);
        FileUtil.recursiveDelete(datastore);

        UUIDUtility uuidUtility = new UUIDUtility();

        StarterData starterData = new StarterData(datastore, uuidUtility)
                .init()
                .authoringSTAMP(
                        TinkarTerm.ACTIVE_STATE,
                        System.currentTimeMillis(),
                        TinkarTerm.USER,
                        TinkarTerm.PRIMORDIAL_MODULE,
                        TinkarTerm.PRIMORDIAL_PATH);

        //Create Description Pattern
        starterData.pattern(TinkarTerm.DESCRIPTION_PATTERN)
                .meaning(TinkarTerm.DESCRIPTION_SEMANTIC)
                .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fieldDefinition(
                        TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION,
                        TinkarTerm.LANGUAGE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.TEXT_FOR_DESCRIPTION,
                        TinkarTerm.DESCRIPTION,
                        TinkarTerm.STRING)
                .fieldDefinition(
                        TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE,
                        TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.DESCRIPTION_TYPE,
                        TinkarTerm.DESCRIPTION_TYPE,
                        TinkarTerm.COMPONENT_FIELD)
                .build();

        //Create Stated Navigation Pattern
        starterData.pattern(TinkarTerm.STATED_NAVIGATION_PATTERN)
                .meaning(TinkarTerm.IS_A)
                .purpose(TinkarTerm.IS_A)
                .fieldDefinition(
                        TinkarTerm.RELATIONSHIP_DESTINATION,
                        TinkarTerm.IS_A,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .fieldDefinition(
                        TinkarTerm.RELATIONSHIP_ORIGIN,
                        TinkarTerm.IS_A,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .build();

        //Create Inferred Navigation Pattern
        starterData.pattern(TinkarTerm.INFERRED_NAVIGATION_PATTERN)
                .meaning(TinkarTerm.IS_A)
                .purpose(TinkarTerm.IS_A)
                .fieldDefinition(
                        TinkarTerm.RELATIONSHIP_DESTINATION,
                        TinkarTerm.IS_A,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .fieldDefinition(
                        TinkarTerm.RELATIONSHIP_ORIGIN,
                        TinkarTerm.IS_A,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .build();

        //Create Identifier Pattern
        starterData.pattern(StarterData.identifierPattern)
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.STRING)
                .build();

        //Create US Dialect Pattern
        starterData.pattern(TinkarTerm.US_DIALECT_PATTERN)
                .meaning(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fieldDefinition(
                        TinkarTerm.US_ENGLISH_DIALECT,
                        TinkarTerm.DESCRIPTION_ACCEPTABILITY,
                        TinkarTerm.COMPONENT_FIELD)
                .build();

        //Create Axiom Syntax Pattern
        Concept axiomSyntax = Concept.make("Axiom Syntax", uuidUtility.createUUID("Axiom Syntax"));
        starterData.concept(axiomSyntax)
                .fullyQualifiedName(axiomSyntax.description(), TinkarTerm.PREFERRED)
                .synonym("Axiom Syntax", TinkarTerm.ACCEPTABLE)
                .definition("Syntax defining description logic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, axiomSyntax.asUuidArray()[0].toString())
                .axiomSyntax("ClassOF(adfasdfasdfasdf)")
                .build();
        Concept expressAxiom = Concept.make("Express axiom syntax", uuidUtility.createUUID("Express axiom syntax"));
        starterData.concept(expressAxiom)
                .fullyQualifiedName(expressAxiom.description(), TinkarTerm.PREFERRED)
                .synonym("Express Axiom", TinkarTerm.ACCEPTABLE)
                .definition("Expressing description logic through syntax", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, expressAxiom.asUuidArray()[0].toString())
                .axiomSyntax("ClassOF(323333j33)")
                .build();
        starterData.pattern(StarterData.axiomSyntaxPattern)
                .meaning(axiomSyntax)
                .purpose(expressAxiom)
                .fieldDefinition(
                        axiomSyntax,
                        expressAxiom,
                        TinkarTerm.STRING)
                .build();

        //Create Stated Definition Pattern
        starterData.pattern(TinkarTerm.EL_PLUS_PLUS_STATED_AXIOMS_PATTERN)
                .meaning(TinkarTerm.DESCRIPTUM)
                .purpose(TinkarTerm.LOGICAL_DEFINITION)
                .fieldDefinition(
                        TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS,
                        TinkarTerm.LOGICAL_DEFINITION,
                        TinkarTerm.DITREE_FIELD)
                .build();

        //Create Inferred Definition Pattern
        starterData.pattern(TinkarTerm.EL_PLUS_PLUS_INFERRED_AXIOMS_PATTERN)
                .meaning(TinkarTerm.DESCRIPTUM)
                .purpose(TinkarTerm.LOGICAL_DEFINITION)
                .fieldDefinition(
                        TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS,
                        TinkarTerm.LOGICAL_DEFINITION,
                        TinkarTerm.DITREE_FIELD)
                .build();

        //Create Path Membership Pattern
        starterData.pattern(StarterData.pathMembershipPattern)
                .meaning(TinkarTerm.PATH)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        //Create STAMP Pattern
        starterData.pattern(TinkarTerm.STAMP_PATTERN)
                .meaning(TinkarTerm.VERSION_PROPERTIES)
                .purpose(TinkarTerm.VERSION_PROPERTIES)
                .fieldDefinition(
                        TinkarTerm.STATUS_VALUE,
                        TinkarTerm.STATUS_FOR_VERSION,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.TIME_FOR_VERSION,
                        TinkarTerm.TIME_FOR_VERSION,
                        TinkarTerm.LONG)
                .fieldDefinition(
                        TinkarTerm.AUTHOR_FOR_VERSION,
                        TinkarTerm.AUTHOR_FOR_VERSION,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.MODULE_FOR_VERSION,
                        TinkarTerm.MODULE_FOR_VERSION,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.PATH_FOR_VERSION,
                        TinkarTerm.PATH_FOR_VERSION,
                        TinkarTerm.COMPONENT_FIELD)
                .build();

        //Create Tinkar base model component pattern
        starterData.pattern(TinkarTerm.TINKAR_BASE_MODEL_COMPONENT_PATTERN)
                .meaning(TinkarTerm.PATH)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        //Create Version control path origin pattern
        starterData.pattern(StarterData.versionControlPathOriginPattern)
                .meaning(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH)
                .purpose(TinkarTerm.PATH_ORIGINS)
                .fieldDefinition(
                        TinkarTerm.PATH_CONCEPT,
                        TinkarTerm.PATH_CONCEPT,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.PATH_ORIGINS,
                        TinkarTerm.PATH_ORIGINS,
                        TinkarTerm.INSTANT_LITERAL) //TODO-aks8m: Check with Keith
                .build();

        //Create Comment Pattern
        starterData.pattern(TinkarTerm.COMMENT_PATTERN)
                .meaning(TinkarTerm.COMMENT)
                .purpose(TinkarTerm.COMMENT)
                .fieldDefinition(
                        TinkarTerm.COMMENT,
                        TinkarTerm.COMMENT,
                        TinkarTerm.STRING)
                .build();

        //Create Komet base model component pattern
        starterData.pattern(TinkarTerm.KOMET_BASE_MODEL_COMPONENT_PATTERN)
                .meaning(TinkarTerm.PATH)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();


        Concept iThinkConcept = Concept.make("I Think", uuidUtility.createUUID("I Think"));
        Concept thisIsWorking = Concept.make("This is Almost Working!", uuidUtility.createUUID("This is Almost Working!"));


        starterData.concept(TinkarTerm.SOLOR_CONCEPT)
                .fullyQualifiedName(TinkarTerm.SOLOR_CONCEPT.description(), TinkarTerm.PREFERRED)
                .synonym("Integrated Knowledge Management", TinkarTerm.PREFERRED)
                .definition("Harmonizing knowledge together", TinkarTerm.PREFERRED)
                .inferredNavigation(List.of(iThinkConcept), null)
                .statedNavigation(List.of(iThinkConcept), null)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SOLOR_CONCEPT.asUuidArray()[0].toString())
                .build();

        starterData.concept(iThinkConcept)
                .fullyQualifiedName(iThinkConcept.description(), TinkarTerm.PREFERRED)
                .synonym("Thinking", TinkarTerm.PREFERRED)
                .definition("That state of trying to figure things out", TinkarTerm.PREFERRED)
                .inferredNavigation(List.of(thisIsWorking), List.of(TinkarTerm.SOLOR_CONCEPT))
                .statedNavigation(List.of(thisIsWorking), List.of(TinkarTerm.SOLOR_CONCEPT))
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, iThinkConcept.asUuidArray()[0].toString())
                .build();

        starterData.concept(thisIsWorking)
                .fullyQualifiedName(thisIsWorking.description(), TinkarTerm.PREFERRED)
                .synonym("Working", TinkarTerm.PREFERRED)
                .definition("The state of things being as they should", TinkarTerm.PREFERRED)
                .inferredNavigation(null, List.of(iThinkConcept))
                .statedNavigation(null, List.of(iThinkConcept))
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, thisIsWorking.asUuidArray()[0].toString())
                .build();

//        for(Field field : TinkarTerm.class.getDeclaredFields()){
//            System.out.println("starterData.concept(TinkarTerm." + field.getName() + ").fullyQualifiedName(TinkarTerm."+ field.getName() +".description(), TinkarTerm.PREFERRED).build();");
//        }

//        starterData.concept(TinkarTerm.STAMP_PATTERN).fullyQualifiedName(TinkarTerm.STAMP_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.DESCRIPTION_PATTERN).fullyQualifiedName(TinkarTerm.DESCRIPTION_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.GB_DIALECT_PATTERN).fullyQualifiedName(TinkarTerm.GB_DIALECT_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.US_DIALECT_PATTERN).fullyQualifiedName(TinkarTerm.US_DIALECT_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.INFERRED_NAVIGATION_PATTERN).fullyQualifiedName(TinkarTerm.INFERRED_NAVIGATION_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.STATED_NAVIGATION_PATTERN).fullyQualifiedName(TinkarTerm.STATED_NAVIGATION_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_AXIOMS_PATTERN).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_STATED_AXIOMS_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_AXIOMS_PATTERN).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_INFERRED_AXIOMS_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.TINKAR_BASE_MODEL_COMPONENT_PATTERN).fullyQualifiedName(TinkarTerm.TINKAR_BASE_MODEL_COMPONENT_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.KOMET_BASE_MODEL_COMPONENT_PATTERN).fullyQualifiedName(TinkarTerm.KOMET_BASE_MODEL_COMPONENT_PATTERN.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.COMMENT_PATTERN).fullyQualifiedName(TinkarTerm.COMMENT_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ACCEPTABLE).fullyQualifiedName(TinkarTerm.ACCEPTABLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ACTION_NAME).fullyQualifiedName(TinkarTerm.ACTION_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ACTION_PROPERTIES).fullyQualifiedName(TinkarTerm.ACTION_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ACTION_PURPOSE).fullyQualifiedName(TinkarTerm.ACTION_PURPOSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ACTIVE_INGREDIENT_IS_DIFFERENT).fullyQualifiedName(TinkarTerm.ACTIVE_INGREDIENT_IS_DIFFERENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ACTIVE_ONLY_DESCRIPTION_LUCENE_MATCH____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ACTIVE_ONLY_DESCRIPTION_LUCENE_MATCH____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ACTIVE_ONLY_DESCRIPTION_REGEX_MATCH____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ACTIVE_ONLY_DESCRIPTION_REGEX_MATCH____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ACTIVE_STATE).fullyQualifiedName(TinkarTerm.ACTIVE_STATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ACTIVITIES_PANEL).fullyQualifiedName(TinkarTerm.ACTIVITIES_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ALL_CHILD_CRITERION_ARE_SATISFIED_FOR_COMPONENT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ALL_CHILD_CRITERION_ARE_SATISFIED_FOR_COMPONENT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ALLERGEN).fullyQualifiedName(TinkarTerm.ALLERGEN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE).fullyQualifiedName(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AND).fullyQualifiedName(TinkarTerm.AND.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ANNOTATION_TYPE).fullyQualifiedName(TinkarTerm.ANNOTATION_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ANONYMOUS_CONCEPT).fullyQualifiedName(TinkarTerm.ANONYMOUS_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ANY_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.ANY_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ANY_CHILD_CRITERION_IS_SATISFIED_FOR_COMPONENT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ANY_CHILD_CRITERION_IS_SATISFIED_FOR_COMPONENT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ANY_COMPONENT).fullyQualifiedName(TinkarTerm.ANY_COMPONENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.APACHE_2_LICENSE).fullyQualifiedName(TinkarTerm.APACHE_2_LICENSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ARRAY).fullyQualifiedName(TinkarTerm.ARRAY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ARRAY_FIELD).fullyQualifiedName(TinkarTerm.ARRAY_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE).fullyQualifiedName(TinkarTerm.ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_1_TO_JOIN).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_1_TO_JOIN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_2_TO_JOIN).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_2_TO_JOIN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_LUCENE_MATCH____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_LUCENE_MATCH____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_CONTAINS_COMPONENT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_CONTAINS_COMPONENT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_CONTAINS_CONCEPT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_CONTAINS_CONCEPT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_CONTAINS_KIND_OF_CONCEPT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_CONTAINS_KIND_OF_CONCEPT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_CONTAINS_STRING____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_CONTAINS_STRING____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_FOR_ACTION).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_FOR_ACTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_FOR_CONSTRAINT).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_FOR_CONSTRAINT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_LIST_FOR_QUERY).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_LIST_FOR_QUERY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_MEMBERSHIP_TYPE).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_MEMBERSHIP_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_NID_FOR_COMPONENT).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_NID_FOR_COMPONENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_PANEL).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSEMBLAGE_RELATED_TO_PATH_MANAGEMENT).fullyQualifiedName(TinkarTerm.ASSEMBLAGE_RELATED_TO_PATH_MANAGEMENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSOCIATED_PARAMETER____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ASSOCIATED_PARAMETER____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSOCIATION_ID).fullyQualifiedName(TinkarTerm.ASSOCIATION_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ASSOCIATION_SEMANTIC).fullyQualifiedName(TinkarTerm.ASSOCIATION_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE).fullyQualifiedName(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AUTHOR_FOR_VERSION).fullyQualifiedName(TinkarTerm.AUTHOR_FOR_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE).fullyQualifiedName(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AUTOMATION_ISSUE).fullyQualifiedName(TinkarTerm.AUTOMATION_ISSUE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AUTOMATION_ISSUE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.AUTOMATION_ISSUE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AUTOMATION_RULE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.AUTOMATION_RULE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.AXIOM_ATTACHMENT_ORDER_OPTIONS_PATTERN).fullyQualifiedName(TinkarTerm.AXIOM_ATTACHMENT_ORDER_OPTIONS_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AXIOM_FOCUS).fullyQualifiedName(TinkarTerm.AXIOM_FOCUS.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.AXIOM_ORDER_OPTIONS_PATTERN).fullyQualifiedName(TinkarTerm.AXIOM_ORDER_OPTIONS_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AXIOM_ORIGIN).fullyQualifiedName(TinkarTerm.AXIOM_ORIGIN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BODY_STRUCTURE).fullyQualifiedName(TinkarTerm.BODY_STRUCTURE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BOOLEAN_FIELD).fullyQualifiedName(TinkarTerm.BOOLEAN_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BOOLEAN_LITERAL).fullyQualifiedName(TinkarTerm.BOOLEAN_LITERAL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BOOLEAN_REFERENCE).fullyQualifiedName(TinkarTerm.BOOLEAN_REFERENCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BOOLEAN_SUBSTITUTION).fullyQualifiedName(TinkarTerm.BOOLEAN_SUBSTITUTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BOSS_SUBSTANCES_ARE_DIFFERENT).fullyQualifiedName(TinkarTerm.BOSS_SUBSTANCES_ARE_DIFFERENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BROAD_TO_NARROW).fullyQualifiedName(TinkarTerm.BROAD_TO_NARROW.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BUSINESS_RULES).fullyQualifiedName(TinkarTerm.BUSINESS_RULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BYTE_ARRAY_FIELD).fullyQualifiedName(TinkarTerm.BYTE_ARRAY_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CPT_MODULES).fullyQualifiedName(TinkarTerm.CPT_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CVX_CODE).fullyQualifiedName(TinkarTerm.CVX_CODE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CVX_DEFINITION_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.CVX_DEFINITION_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CVX_DESCRIPTION_ID).fullyQualifiedName(TinkarTerm.CVX_DESCRIPTION_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CVX_MODULES).fullyQualifiedName(TinkarTerm.CVX_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CANCELED_STATE).fullyQualifiedName(TinkarTerm.CANCELED_STATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CASE_INSENSITIVE_EVALUATION).fullyQualifiedName(TinkarTerm.CASE_INSENSITIVE_EVALUATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CASE_SENSITIVE_EVALUATION).fullyQualifiedName(TinkarTerm.CASE_SENSITIVE_EVALUATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION).fullyQualifiedName(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CENTER_PANE_DEFAULTS).fullyQualifiedName(TinkarTerm.CENTER_PANE_DEFAULTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CENTER_PANE_OPTIONS).fullyQualifiedName(TinkarTerm.CENTER_PANE_OPTIONS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CENTER_TAB_NODES).fullyQualifiedName(TinkarTerm.CENTER_TAB_NODES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CHARACTERISTIC_NID_FOR_RF2_RELATIONSHIP).fullyQualifiedName(TinkarTerm.CHARACTERISTIC_NID_FOR_RF2_RELATIONSHIP.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CHILD_OF____INTERNAL_USE_RELATIONSHIP_TYPE).fullyQualifiedName(TinkarTerm.CHILD_OF____INTERNAL_USE_RELATIONSHIP_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CHINESE_LANGUAGE).fullyQualifiedName(TinkarTerm.CHINESE_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CHRONICLE_PROPERTIES).fullyQualifiedName(TinkarTerm.CHRONICLE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CIRCUMSTANCE_PROPERTIES).fullyQualifiedName(TinkarTerm.CIRCUMSTANCE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CLASSIFICATION_RESULTS_PANEL).fullyQualifiedName(TinkarTerm.CLASSIFICATION_RESULTS_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CLASSIFIER_FOR_LOGIC_COORDINATE).fullyQualifiedName(TinkarTerm.CLASSIFIER_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CLINICAL_STATEMENT_ISSUE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.CLINICAL_STATEMENT_ISSUE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CLINICAL_STATEMENT_PROPERTIES).fullyQualifiedName(TinkarTerm.CLINICAL_STATEMENT_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CLINVAR_DEFINITION_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.CLINVAR_DEFINITION_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CLINVAR_DESCRIPTION_ID).fullyQualifiedName(TinkarTerm.CLINVAR_DESCRIPTION_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CLINVAR_GENE_TO_PHENOTYPE_NON_DEFINING_TAXONOMY).fullyQualifiedName(TinkarTerm.CLINVAR_GENE_TO_PHENOTYPE_NON_DEFINING_TAXONOMY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CLINVAR_VARIANT_ID).fullyQualifiedName(TinkarTerm.CLINVAR_VARIANT_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CLINVAR_VARIANT_TO_GENE_NON_DEFINING_TAXONOMY).fullyQualifiedName(TinkarTerm.CLINVAR_VARIANT_TO_GENE_NON_DEFINING_TAXONOMY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CODE).fullyQualifiedName(TinkarTerm.CODE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COLUMN_DEFAULT_VALUE).fullyQualifiedName(TinkarTerm.COLUMN_DEFAULT_VALUE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COLUMN_NAME).fullyQualifiedName(TinkarTerm.COLUMN_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COLUMN_ORDER).fullyQualifiedName(TinkarTerm.COLUMN_ORDER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COLUMN_REQUIRED).fullyQualifiedName(TinkarTerm.COLUMN_REQUIRED.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COLUMN_TYPE).fullyQualifiedName(TinkarTerm.COLUMN_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COLUMN_VALIDATOR).fullyQualifiedName(TinkarTerm.COLUMN_VALIDATOR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COLUMN_VALIDATOR_DATA).fullyQualifiedName(TinkarTerm.COLUMN_VALIDATOR_DATA.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMMENT).fullyQualifiedName(TinkarTerm.COMMENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMMITTED_STATE_FOR_CHRONICLE).fullyQualifiedName(TinkarTerm.COMMITTED_STATE_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMMITTED_STATE_FOR_VERSION).fullyQualifiedName(TinkarTerm.COMMITTED_STATE_FOR_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMMUNICATE_A_CONCEPT_WITH_SPEECH_OR_WRITING).fullyQualifiedName(TinkarTerm.COMMUNICATE_A_CONCEPT_WITH_SPEECH_OR_WRITING.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPLETION_PANEL).fullyQualifiedName(TinkarTerm.COMPLETION_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_ID_LIST_FIELD).fullyQualifiedName(TinkarTerm.COMPONENT_ID_LIST_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_ID_SET_FIELD).fullyQualifiedName(TinkarTerm.COMPONENT_ID_SET_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_FIELD).fullyQualifiedName(TinkarTerm.COMPONENT_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_FOR_SEMANTIC).fullyQualifiedName(TinkarTerm.COMPONENT_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_ID_1).fullyQualifiedName(TinkarTerm.COMPONENT_ID_1.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_ID_2).fullyQualifiedName(TinkarTerm.COMPONENT_ID_2.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_ID_3).fullyQualifiedName(TinkarTerm.COMPONENT_ID_3.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_ID_4).fullyQualifiedName(TinkarTerm.COMPONENT_ID_4.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_ID_5).fullyQualifiedName(TinkarTerm.COMPONENT_ID_5.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_ID_6).fullyQualifiedName(TinkarTerm.COMPONENT_ID_6.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_ID_7).fullyQualifiedName(TinkarTerm.COMPONENT_ID_7.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_IS_NOT_MEMBER_OF____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.COMPONENT_IS_NOT_MEMBER_OF____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_IS_ACTIVE____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.COMPONENT_IS_ACTIVE____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_IS_INACTIVE____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.COMPONENT_IS_INACTIVE____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_IS_MEMBER_OF____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.COMPONENT_IS_MEMBER_OF____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_LIST_PANEL).fullyQualifiedName(TinkarTerm.COMPONENT_LIST_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_SEMANTIC).fullyQualifiedName(TinkarTerm.COMPONENT_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPONENT_TYPE_FOCUS).fullyQualifiedName(TinkarTerm.COMPONENT_TYPE_FOCUS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COMPOSITE_ACTION_PANEL).fullyQualifiedName(TinkarTerm.COMPOSITE_ACTION_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONDOR_CLASSIFIER).fullyQualifiedName(TinkarTerm.CONDOR_CLASSIFIER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.CONCEPT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE).fullyQualifiedName(TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.CONCEPT_ATTACHMENT_ORDER_OPTIONS_PATTERN).fullyQualifiedName(TinkarTerm.CONCEPT_ATTACHMENT_ORDER_OPTIONS_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_BUILDER_PANEL).fullyQualifiedName(TinkarTerm.CONCEPT_BUILDER_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_CONSTRAINTS).fullyQualifiedName(TinkarTerm.CONCEPT_CONSTRAINTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_DETAILS_CLASSIFICATION_RESULTS_LINKED_PANEL).fullyQualifiedName(TinkarTerm.CONCEPT_DETAILS_CLASSIFICATION_RESULTS_LINKED_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_DETAILS_LIST_VIEW_LINKED_PANEL).fullyQualifiedName(TinkarTerm.CONCEPT_DETAILS_LIST_VIEW_LINKED_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_DETAILS_NEW_CONCEPT_LINKED_PANEL).fullyQualifiedName(TinkarTerm.CONCEPT_DETAILS_NEW_CONCEPT_LINKED_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_DETAILS_PANEL).fullyQualifiedName(TinkarTerm.CONCEPT_DETAILS_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_DETAILS_SEARCH_LINKED_PANEL).fullyQualifiedName(TinkarTerm.CONCEPT_DETAILS_SEARCH_LINKED_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_DETAILS_TAXONOMY_LINKED_PANEL).fullyQualifiedName(TinkarTerm.CONCEPT_DETAILS_TAXONOMY_LINKED_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE).fullyQualifiedName(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_FIELD).fullyQualifiedName(TinkarTerm.CONCEPT_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_FOCUS).fullyQualifiedName(TinkarTerm.CONCEPT_FOCUS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_FOR_COMPONENT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.CONCEPT_FOR_COMPONENT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_HAS_TAXONOMY_DISTANCE_FROM).fullyQualifiedName(TinkarTerm.CONCEPT_HAS_TAXONOMY_DISTANCE_FROM.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_IMAGE).fullyQualifiedName(TinkarTerm.CONCEPT_IMAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_IS____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.CONCEPT_IS____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_IS_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.CONCEPT_IS_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_IS_CHILD_OF____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.CONCEPT_IS_CHILD_OF____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_IS_DESCENDENT_OF____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.CONCEPT_IS_DESCENDENT_OF____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_IS_KIND_OF____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.CONCEPT_IS_KIND_OF____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_PROPERTIES).fullyQualifiedName(TinkarTerm.CONCEPT_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_REFERENCE).fullyQualifiedName(TinkarTerm.CONCEPT_REFERENCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_SEMANTIC).fullyQualifiedName(TinkarTerm.CONCEPT_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_SUBSTITUTION).fullyQualifiedName(TinkarTerm.CONCEPT_SUBSTITUTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_TO_FIND).fullyQualifiedName(TinkarTerm.CONCEPT_TO_FIND.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_TYPE).fullyQualifiedName(TinkarTerm.CONCEPT_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCEPT_VERSION).fullyQualifiedName(TinkarTerm.CONCEPT_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONCRETE_DOMAIN_OPERATOR).fullyQualifiedName(TinkarTerm.CONCRETE_DOMAIN_OPERATOR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONDITIONAL_TRIGGERS).fullyQualifiedName(TinkarTerm.CONDITIONAL_TRIGGERS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONFIGURATION_NAME).fullyQualifiedName(TinkarTerm.CONFIGURATION_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONFIGURATION_PROPERTIES).fullyQualifiedName(TinkarTerm.CONFIGURATION_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONNECTIVE_OPERATOR).fullyQualifiedName(TinkarTerm.CONNECTIVE_OPERATOR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONTENT_METADATA).fullyQualifiedName(TinkarTerm.CONTENT_METADATA.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONTENT_ISSUE).fullyQualifiedName(TinkarTerm.CONTENT_ISSUE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONTENT_ISSUE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.CONTENT_ISSUE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONTENT_LICENSE).fullyQualifiedName(TinkarTerm.CONTENT_LICENSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONVERTED_IBDF_ARTIFACT_CLASSIFIER).fullyQualifiedName(TinkarTerm.CONVERTED_IBDF_ARTIFACT_CLASSIFIER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONVERTED_IBDF_ARTIFACT_VERSION).fullyQualifiedName(TinkarTerm.CONVERTED_IBDF_ARTIFACT_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CONVERTER_VERSION).fullyQualifiedName(TinkarTerm.CONVERTER_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COPYRIGHT).fullyQualifiedName(TinkarTerm.COPYRIGHT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COPYRIGHT_FREE_WORK).fullyQualifiedName(TinkarTerm.COPYRIGHT_FREE_WORK.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CORELATION_COMPARISON_EXPRESSION).fullyQualifiedName(TinkarTerm.CORELATION_COMPARISON_EXPRESSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CORELATION_EXPRESSION).fullyQualifiedName(TinkarTerm.CORELATION_EXPRESSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CORELATION_REFERENCE_EXPRESSION).fullyQualifiedName(TinkarTerm.CORELATION_REFERENCE_EXPRESSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CORRELATION_PROPERTIES).fullyQualifiedName(TinkarTerm.CORRELATION_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.COUNT_OF_BASE_DIFFERENT).fullyQualifiedName(TinkarTerm.COUNT_OF_BASE_DIFFERENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE).fullyQualifiedName(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CURRENT_ACTIVITY).fullyQualifiedName(TinkarTerm.CURRENT_ACTIVITY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CZECH_DIALECT).fullyQualifiedName(TinkarTerm.CZECH_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CZECH_LANGUAGE).fullyQualifiedName(TinkarTerm.CZECH_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DANISH_LANGUAGE).fullyQualifiedName(TinkarTerm.DANISH_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DATABASE_UUID).fullyQualifiedName(TinkarTerm.DATABASE_UUID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DATASTORE_LOCATION).fullyQualifiedName(TinkarTerm.DATASTORE_LOCATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE).fullyQualifiedName(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DEFINITION_DESCRIPTION_TYPE).fullyQualifiedName(TinkarTerm.DEFINITION_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DEFINITION_ROOT).fullyQualifiedName(TinkarTerm.DEFINITION_ROOT.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.DEPENDENCY_MANAGEMENT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.DEPENDENCY_MANAGEMENT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION).fullyQualifiedName(TinkarTerm.DESCRIPTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_LUCENE_MATCH____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.DESCRIPTION_LUCENE_MATCH____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_ACCEPTABILITY).fullyQualifiedName(TinkarTerm.DESCRIPTION_ACCEPTABILITY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.DESCRIPTION_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.DESCRIPTION_ATTACHMENT_ORDER_OPTIONS_PATTERN).fullyQualifiedName(TinkarTerm.DESCRIPTION_ATTACHMENT_ORDER_OPTIONS_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_CASE_SENSITIVE).fullyQualifiedName(TinkarTerm.DESCRIPTION_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE).fullyQualifiedName(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_CORE_TYPE).fullyQualifiedName(TinkarTerm.DESCRIPTION_CORE_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_DIALECT_PAIR).fullyQualifiedName(TinkarTerm.DESCRIPTION_DIALECT_PAIR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_FOCUS).fullyQualifiedName(TinkarTerm.DESCRIPTION_FOCUS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR).fullyQualifiedName(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE).fullyQualifiedName(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE).fullyQualifiedName(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE).fullyQualifiedName(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_REGEX_MATCH____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.DESCRIPTION_REGEX_MATCH____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_SEMANTIC).fullyQualifiedName(TinkarTerm.DESCRIPTION_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_TYPE).fullyQualifiedName(TinkarTerm.DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION).fullyQualifiedName(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_IN_SOURCE_TERMINOLOGY).fullyQualifiedName(TinkarTerm.DESCRIPTION_TYPE_IN_SOURCE_TERMINOLOGY.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_ORDER_OPTIONS_PATTERN).fullyQualifiedName(TinkarTerm.DESCRIPTION_TYPE_ORDER_OPTIONS_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE).fullyQualifiedName(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES).fullyQualifiedName(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_LOGIC_CLASSIFIER).fullyQualifiedName(TinkarTerm.DESCRIPTION_LOGIC_CLASSIFIER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE).fullyQualifiedName(TinkarTerm.DESCRIPTION_LOGIC_PROFILE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_AND_OR_DIALECT_PROPERTIES).fullyQualifiedName(TinkarTerm.DESCRIPTION_AND_OR_DIALECT_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTUM).fullyQualifiedName(TinkarTerm.DESCRIPTUM.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE).fullyQualifiedName(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESTINATION_NID_FOR_RF2_RELATIONSHIP).fullyQualifiedName(TinkarTerm.DESTINATION_NID_FOR_RF2_RELATIONSHIP.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DETAIL_NODES).fullyQualifiedName(TinkarTerm.DETAIL_NODES.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.DETAIL_ORDER_OPTIONS_PATTERN).fullyQualifiedName(TinkarTerm.DETAIL_ORDER_OPTIONS_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DETAIL_PANE_AXIOM_ORDER).fullyQualifiedName(TinkarTerm.DETAIL_PANE_AXIOM_ORDER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DETAIL_PANE_DESCRIPTION_TYPE_ORDER).fullyQualifiedName(TinkarTerm.DETAIL_PANE_DESCRIPTION_TYPE_ORDER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DETAIL_PANE_ORDER).fullyQualifiedName(TinkarTerm.DETAIL_PANE_ORDER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DEVELOPMENT_MODULE).fullyQualifiedName(TinkarTerm.DEVELOPMENT_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DEVELOPMENT_PATH).fullyQualifiedName(TinkarTerm.DEVELOPMENT_PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DIGRAPH_FIELD).fullyQualifiedName(TinkarTerm.DIGRAPH_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DITREE_FIELD).fullyQualifiedName(TinkarTerm.DITREE_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DIALECT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.DIALECT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE).fullyQualifiedName(TinkarTerm.DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR).fullyQualifiedName(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE).fullyQualifiedName(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DIGRAPH_FOR_MANIFOLD).fullyQualifiedName(TinkarTerm.DIGRAPH_FOR_MANIFOLD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DIRECTED_GRAPH).fullyQualifiedName(TinkarTerm.DIRECTED_GRAPH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DISCRETE_MEASURE_SEMANTIC).fullyQualifiedName(TinkarTerm.DISCRETE_MEASURE_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DISJOINT_WITH).fullyQualifiedName(TinkarTerm.DISJOINT_WITH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DISPLAY_FIELDS).fullyQualifiedName(TinkarTerm.DISPLAY_FIELDS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DOSE_FORM_IS_DIFFERENT).fullyQualifiedName(TinkarTerm.DOSE_FORM_IS_DIFFERENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DOUBLE_FIELD).fullyQualifiedName(TinkarTerm.DOUBLE_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DUTCH_LANGUAGE).fullyQualifiedName(TinkarTerm.DUTCH_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_ASSEMBLAGE_DEFINITION_PANEL).fullyQualifiedName(TinkarTerm.DYNAMIC_ASSEMBLAGE_DEFINITION_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_ASSEMBLAGES).fullyQualifiedName(TinkarTerm.DYNAMIC_ASSEMBLAGES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES).fullyQualifiedName(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_COLUMNS).fullyQualifiedName(TinkarTerm.DYNAMIC_COLUMNS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_DEFINITION_DESCRIPTION).fullyQualifiedName(TinkarTerm.DYNAMIC_DEFINITION_DESCRIPTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_EXTENSION_DEFINITION).fullyQualifiedName(TinkarTerm.DYNAMIC_EXTENSION_DEFINITION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_METADATA).fullyQualifiedName(TinkarTerm.DYNAMIC_METADATA.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_NAMESPACE).fullyQualifiedName(TinkarTerm.DYNAMIC_NAMESPACE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_REFERENCED_COMPONENT_RESTRICTION).fullyQualifiedName(TinkarTerm.DYNAMIC_REFERENCED_COMPONENT_RESTRICTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DYNAMIC_SEMANTIC).fullyQualifiedName(TinkarTerm.DYNAMIC_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EL_PROFILE_SET_OPERATOR).fullyQualifiedName(TinkarTerm.EL_PROFILE_SET_OPERATOR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_DIGRAPH).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_INFERRED_DIGRAPH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_DIGRAPH).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_STATED_DIGRAPH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EL_PLUS_PLUS_DIGRAPH).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_DIGRAPH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EL_PLUS_PLUS_PROFILE).fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_PROFILE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EDITOR_COMMENT).fullyQualifiedName(TinkarTerm.EDITOR_COMMENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EDITOR_COMMENT_CONTEXT).fullyQualifiedName(TinkarTerm.EDITOR_COMMENT_CONTEXT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EFFECTIVE_DATE).fullyQualifiedName(TinkarTerm.EFFECTIVE_DATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ENABLE_CENTER_PANE).fullyQualifiedName(TinkarTerm.ENABLE_CENTER_PANE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ENABLE_EDITING).fullyQualifiedName(TinkarTerm.ENABLE_EDITING.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ENABLE_LEFT_PANE).fullyQualifiedName(TinkarTerm.ENABLE_LEFT_PANE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ENABLE_RIGHT_PANE).fullyQualifiedName(TinkarTerm.ENABLE_RIGHT_PANE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ENCLOSING_CONCEPT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.ENCLOSING_CONCEPT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ENGLISH_DIALECT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.ENGLISH_DIALECT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ENGLISH_LANGUAGE).fullyQualifiedName(TinkarTerm.ENGLISH_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ENTRY_SEQUENCE_FOR_COMPONENT).fullyQualifiedName(TinkarTerm.ENTRY_SEQUENCE_FOR_COMPONENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EQUAL_TO).fullyQualifiedName(TinkarTerm.EQUAL_TO.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EQUIVALENCE_TYPE).fullyQualifiedName(TinkarTerm.EQUIVALENCE_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EQUIVALENCE_TYPES).fullyQualifiedName(TinkarTerm.EQUIVALENCE_TYPES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EVENT).fullyQualifiedName(TinkarTerm.EVENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EVENT_DURATION).fullyQualifiedName(TinkarTerm.EVENT_DURATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EVENT_FREQUENCY).fullyQualifiedName(TinkarTerm.EVENT_FREQUENCY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EVENT_SEPARATION).fullyQualifiedName(TinkarTerm.EVENT_SEPARATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXACT).fullyQualifiedName(TinkarTerm.EXACT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXISTENTIAL_MEASUREMENT_SEMANTIC).fullyQualifiedName(TinkarTerm.EXISTENTIAL_MEASUREMENT_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXISTENTIAL_RESTRICTION).fullyQualifiedName(TinkarTerm.EXISTENTIAL_RESTRICTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXPLORATION_NODES).fullyQualifiedName(TinkarTerm.EXPLORATION_NODES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXPORT_SPECIFICATION_PANEL).fullyQualifiedName(TinkarTerm.EXPORT_SPECIFICATION_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXTENDED_DESCRIPTION_TYPE).fullyQualifiedName(TinkarTerm.EXTENDED_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE).fullyQualifiedName(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXTENDED_SEARCH_PANEL).fullyQualifiedName(TinkarTerm.EXTENDED_SEARCH_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXTERNAL_DATA_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.EXTERNAL_DATA_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.EXTERNAL_USER_ID).fullyQualifiedName(TinkarTerm.EXTERNAL_USER_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FHIR_URI).fullyQualifiedName(TinkarTerm.FHIR_URI.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FHIR_MODULES).fullyQualifiedName(TinkarTerm.FHIR_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FLWOR_QUERY_PANEL).fullyQualifiedName(TinkarTerm.FLWOR_QUERY_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FATHER_OF_SUBJECT_OF_RECORD).fullyQualifiedName(TinkarTerm.FATHER_OF_SUBJECT_OF_RECORD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FEATURE).fullyQualifiedName(TinkarTerm.FEATURE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FEATURE_TYPE).fullyQualifiedName(TinkarTerm.FEATURE_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FIELD_1_TO_JOIN).fullyQualifiedName(TinkarTerm.FIELD_1_TO_JOIN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FIELD_2_TO_JOIN).fullyQualifiedName(TinkarTerm.FIELD_2_TO_JOIN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FIELD_SUBSTITUTION).fullyQualifiedName(TinkarTerm.FIELD_SUBSTITUTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_COORDINATE).fullyQualifiedName(TinkarTerm.FILTER_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_COORDINATE_FOR_TAXONOMY_COORDINATE).fullyQualifiedName(TinkarTerm.FILTER_COORDINATE_FOR_TAXONOMY_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_COORDINATE_NAME).fullyQualifiedName(TinkarTerm.FILTER_COORDINATE_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_COORDINATE_PROPERTIES).fullyQualifiedName(TinkarTerm.FILTER_COORDINATE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_FOR_LANGUAGE).fullyQualifiedName(TinkarTerm.FILTER_FOR_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_FOR_VERTEX).fullyQualifiedName(TinkarTerm.FILTER_FOR_VERTEX.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_FOR_VIEW).fullyQualifiedName(TinkarTerm.FILTER_FOR_VIEW.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_POSITION_FOR_STAMP_COORDINATE).fullyQualifiedName(TinkarTerm.FILTER_POSITION_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_PRECEDENCE_FOR_STAMP_COORDINATE).fullyQualifiedName(TinkarTerm.FILTER_PRECEDENCE_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FILTER_SEQUENCE_FOR_VERSION).fullyQualifiedName(TinkarTerm.FILTER_SEQUENCE_FOR_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FINDING).fullyQualifiedName(TinkarTerm.FINDING.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FLOAT_FIELD).fullyQualifiedName(TinkarTerm.FLOAT_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FLOAT_LITERAL).fullyQualifiedName(TinkarTerm.FLOAT_LITERAL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FLOAT_SUBSTITUTION).fullyQualifiedName(TinkarTerm.FLOAT_SUBSTITUTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FOR_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.FOR_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FORCE).fullyQualifiedName(TinkarTerm.FORCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FRENCH_DIALECT).fullyQualifiedName(TinkarTerm.FRENCH_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FRENCH_LANGUAGE).fullyQualifiedName(TinkarTerm.FRENCH_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE).fullyQualifiedName(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FULLY_QUALIFIED_NAME_FOR_CONCEPT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.FULLY_QUALIFIED_NAME_FOR_CONCEPT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FUNCTIONAL_FEATURE).fullyQualifiedName(TinkarTerm.FUNCTIONAL_FEATURE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GB_ENGLISH_DIALECT).fullyQualifiedName(TinkarTerm.GB_ENGLISH_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GEM_FLAGS).fullyQualifiedName(TinkarTerm.GEM_FLAGS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GENERATED_ADMINISTRATION_OF_MODULE).fullyQualifiedName(TinkarTerm.GENERATED_ADMINISTRATION_OF_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GERMAN_LANGUAGE).fullyQualifiedName(TinkarTerm.GERMAN_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GIT_LOCAL_FOLDER).fullyQualifiedName(TinkarTerm.GIT_LOCAL_FOLDER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GIT_PASSWORD).fullyQualifiedName(TinkarTerm.GIT_PASSWORD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GIT_URL).fullyQualifiedName(TinkarTerm.GIT_URL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GIT_USER_NAME).fullyQualifiedName(TinkarTerm.GIT_USER_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GREATER_THAN).fullyQualifiedName(TinkarTerm.GREATER_THAN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GREATER_THAN_OR_EQUAL_TO).fullyQualifiedName(TinkarTerm.GREATER_THAN_OR_EQUAL_TO.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GROOVY_SCRIPTING_PANEL).fullyQualifiedName(TinkarTerm.GROOVY_SCRIPTING_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.GROUPING).fullyQualifiedName(TinkarTerm.GROUPING.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.HL7_V3_MODULES).fullyQualifiedName(TinkarTerm.HL7_V3_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.HAS_ACTIVE_INGREDIENT).fullyQualifiedName(TinkarTerm.HAS_ACTIVE_INGREDIENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.HAS_DOSE_FORM).fullyQualifiedName(TinkarTerm.HAS_DOSE_FORM.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.HEALTH_CONCEPT).fullyQualifiedName(TinkarTerm.HEALTH_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.HEALTH_RISK).fullyQualifiedName(TinkarTerm.HEALTH_RISK.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ICD10_MODULES).fullyQualifiedName(TinkarTerm.ICD10_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IHTSDO_CLASSIFIER).fullyQualifiedName(TinkarTerm.IHTSDO_CLASSIFIER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ISO_8601_INTERVAL_AFTER_STATEMENT_TIME).fullyQualifiedName(TinkarTerm.ISO_8601_INTERVAL_AFTER_STATEMENT_TIME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ISO_8601_INTERVAL_PRIOR_TO_STATEMENT_TIME).fullyQualifiedName(TinkarTerm.ISO_8601_INTERVAL_PRIOR_TO_STATEMENT_TIME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ISO_8601_REPRESENTATION_OF_DATES_AND_TIMES).fullyQualifiedName(TinkarTerm.ISO_8601_REPRESENTATION_OF_DATES_AND_TIMES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IDENTIFIER_SOURCE).fullyQualifiedName(TinkarTerm.IDENTIFIER_SOURCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IMAGE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.IMAGE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IMAGE_DATA_FOR_SEMANTIC).fullyQualifiedName(TinkarTerm.IMAGE_DATA_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IMAGE_FIELD).fullyQualifiedName(TinkarTerm.IMAGE_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IMAGE_SEMANTIC).fullyQualifiedName(TinkarTerm.IMAGE_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IMMEDIATE).fullyQualifiedName(TinkarTerm.IMMEDIATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES).fullyQualifiedName(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IMPORT_SPECIFICATION_PANEL).fullyQualifiedName(TinkarTerm.IMPORT_SPECIFICATION_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INACTIVE_STATE).fullyQualifiedName(TinkarTerm.INACTIVE_STATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INCLUDE_DEFINING_TAXONOMY).fullyQualifiedName(TinkarTerm.INCLUDE_DEFINING_TAXONOMY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INCLUDE_LOWER_BOUND).fullyQualifiedName(TinkarTerm.INCLUDE_LOWER_BOUND.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INCLUDE_UPPER_BOUND).fullyQualifiedName(TinkarTerm.INCLUDE_UPPER_BOUND.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE).fullyQualifiedName(TinkarTerm.INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.INFERRED_NAVIGATION).fullyQualifiedName(TinkarTerm.INFERRED_NAVIGATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INFERRED_PREMISE_TYPE).fullyQualifiedName(TinkarTerm.INFERRED_PREMISE_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INGREDIENT_STRENGTH).fullyQualifiedName(TinkarTerm.INGREDIENT_STRENGTH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INSTANCE_MODE).fullyQualifiedName(TinkarTerm.INSTANCE_MODE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INSTANT_LITERAL).fullyQualifiedName(TinkarTerm.INSTANT_LITERAL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INSTANT_SUBSTITUTION).fullyQualifiedName(TinkarTerm.INSTANT_SUBSTITUTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_1).fullyQualifiedName(TinkarTerm.INTEGER_1.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_2).fullyQualifiedName(TinkarTerm.INTEGER_2.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_3).fullyQualifiedName(TinkarTerm.INTEGER_3.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_4).fullyQualifiedName(TinkarTerm.INTEGER_4.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_5).fullyQualifiedName(TinkarTerm.INTEGER_5.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_6).fullyQualifiedName(TinkarTerm.INTEGER_6.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_7).fullyQualifiedName(TinkarTerm.INTEGER_7.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_FIELD).fullyQualifiedName(TinkarTerm.INTEGER_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_LITERAL).fullyQualifiedName(TinkarTerm.INTEGER_LITERAL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_REFERENCE).fullyQualifiedName(TinkarTerm.INTEGER_REFERENCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_SEMANTIC).fullyQualifiedName(TinkarTerm.INTEGER_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTEGER_SUBSTITUTION).fullyQualifiedName(TinkarTerm.INTEGER_SUBSTITUTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTERVAL_PROPERTIES).fullyQualifiedName(TinkarTerm.INTERVAL_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTERVENTION_RESULT_STATUS).fullyQualifiedName(TinkarTerm.INTERVENTION_RESULT_STATUS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INTRINSIC_ROLE).fullyQualifiedName(TinkarTerm.INTRINSIC_ROLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INVERSE_FEATURE).fullyQualifiedName(TinkarTerm.INVERSE_FEATURE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INVERSE_NAME).fullyQualifiedName(TinkarTerm.INVERSE_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.INVERSE_TREE_LIST).fullyQualifiedName(TinkarTerm.INVERSE_TREE_LIST.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IRISH_DIALECT).fullyQualifiedName(TinkarTerm.IRISH_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IRISH_LANGUAGE).fullyQualifiedName(TinkarTerm.IRISH_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IS_A).fullyQualifiedName(TinkarTerm.IS_A.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IS_A_INFERRED_NAVIGATION).fullyQualifiedName(TinkarTerm.IS_A_INFERRED_NAVIGATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.IS_A_STATED_NAVIGATION).fullyQualifiedName(TinkarTerm.IS_A_STATED_NAVIGATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ISSUE_MANAGEMENT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.ISSUE_MANAGEMENT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ITALIAN_LANGUAGE).fullyQualifiedName(TinkarTerm.ITALIAN_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ITEM_ACTIVE).fullyQualifiedName(TinkarTerm.ITEM_ACTIVE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ITEM_COUNT).fullyQualifiedName(TinkarTerm.ITEM_COUNT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ITEM_NAME).fullyQualifiedName(TinkarTerm.ITEM_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.JAPANESE_LANGUAGE).fullyQualifiedName(TinkarTerm.JAPANESE_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.JOIN_QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.JOIN_QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOMET_ISSUE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.KOMET_ISSUE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOMET_MODULE).fullyQualifiedName(TinkarTerm.KOMET_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOMET_PREFERENCE_PROPERTIES).fullyQualifiedName(TinkarTerm.KOMET_PREFERENCE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOMET_USER).fullyQualifiedName(TinkarTerm.KOMET_USER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOMET_USER_LIST).fullyQualifiedName(TinkarTerm.KOMET_USER_LIST.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOMET_ENVIRONMENT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.KOMET_ENVIRONMENT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOMET_ISSUE).fullyQualifiedName(TinkarTerm.KOMET_ISSUE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOMET_PANELS).fullyQualifiedName(TinkarTerm.KOMET_PANELS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOREAN_DIALECT).fullyQualifiedName(TinkarTerm.KOREAN_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.KOREAN_LANGUAGE).fullyQualifiedName(TinkarTerm.KOREAN_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.LIVD_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_EQUIPMENT_UID).fullyQualifiedName(TinkarTerm.LIVD_EQUIPMENT_UID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_EQUIPMENT_UID_TYPE).fullyQualifiedName(TinkarTerm.LIVD_EQUIPMENT_UID_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_MANUFACTURER).fullyQualifiedName(TinkarTerm.LIVD_MANUFACTURER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_MODEL).fullyQualifiedName(TinkarTerm.LIVD_MODEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_PUBLICATION_VERSION_ID).fullyQualifiedName(TinkarTerm.LIVD_PUBLICATION_VERSION_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_SEMANTICS).fullyQualifiedName(TinkarTerm.LIVD_SEMANTICS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_VENDOR_ANALYTE_CODE).fullyQualifiedName(TinkarTerm.LIVD_VENDOR_ANALYTE_CODE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_VENDOR_ANALYTE_NAME).fullyQualifiedName(TinkarTerm.LIVD_VENDOR_ANALYTE_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_VENDOR_COMMENT).fullyQualifiedName(TinkarTerm.LIVD_VENDOR_COMMENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_VENDOR_REFERENCE_ID).fullyQualifiedName(TinkarTerm.LIVD_VENDOR_REFERENCE_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_VENDOR_RESULT_DESCRIPTION).fullyQualifiedName(TinkarTerm.LIVD_VENDOR_RESULT_DESCRIPTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LIVD_VENDOR_SPECIMEN_DESCRIPTION).fullyQualifiedName(TinkarTerm.LIVD_VENDOR_SPECIMEN_DESCRIPTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_ID_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.LOINC_ID_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_COMPONENT).fullyQualifiedName(TinkarTerm.LOINC_COMPONENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_CONCEPT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.LOINC_CONCEPT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_LONG_COMMON_NAME).fullyQualifiedName(TinkarTerm.LOINC_LONG_COMMON_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_METHOD_TYPE).fullyQualifiedName(TinkarTerm.LOINC_METHOD_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_NUMBER).fullyQualifiedName(TinkarTerm.LOINC_NUMBER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_PROPERTY).fullyQualifiedName(TinkarTerm.LOINC_PROPERTY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_RECORD_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.LOINC_RECORD_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_RECORD_ASSEMBLAGE___DYNAMIC).fullyQualifiedName(TinkarTerm.LOINC_RECORD_ASSEMBLAGE___DYNAMIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_SCALE_TYPE).fullyQualifiedName(TinkarTerm.LOINC_SCALE_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_SHORT_NAME).fullyQualifiedName(TinkarTerm.LOINC_SHORT_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_STATUS).fullyQualifiedName(TinkarTerm.LOINC_STATUS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_SYSTEM).fullyQualifiedName(TinkarTerm.LOINC_SYSTEM.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_TIME_ASPECT).fullyQualifiedName(TinkarTerm.LOINC_TIME_ASPECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_ISSUE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.LOINC_ISSUE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_LICENSE).fullyQualifiedName(TinkarTerm.LOINC_LICENSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOINC_MODULES).fullyQualifiedName(TinkarTerm.LOINC_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LANGUAGE).fullyQualifiedName(TinkarTerm.LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION).fullyQualifiedName(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LANGUAGE_COORDINATE_FOR_TAXONOMY_COORDINATE).fullyQualifiedName(TinkarTerm.LANGUAGE_COORDINATE_FOR_TAXONOMY_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LANGUAGE_COORDINATE_KEY_FOR_MANIFOLD).fullyQualifiedName(TinkarTerm.LANGUAGE_COORDINATE_KEY_FOR_MANIFOLD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LANGUAGE_COORDINATE_NAME).fullyQualifiedName(TinkarTerm.LANGUAGE_COORDINATE_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES).fullyQualifiedName(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE).fullyQualifiedName(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE).fullyQualifiedName(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LATERALITY).fullyQualifiedName(TinkarTerm.LATERALITY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LATIN_AMERICAN_SPANISH_DIALECT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.LATIN_AMERICAN_SPANISH_DIALECT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LEFT_PANE_DAFAULTS).fullyQualifiedName(TinkarTerm.LEFT_PANE_DAFAULTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LEFT_PANE_OPTIONS).fullyQualifiedName(TinkarTerm.LEFT_PANE_OPTIONS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LEFT_TAB_NODES).fullyQualifiedName(TinkarTerm.LEFT_TAB_NODES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LESS_THAN).fullyQualifiedName(TinkarTerm.LESS_THAN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LESS_THAN_OR_EQUAL_TO).fullyQualifiedName(TinkarTerm.LESS_THAN_OR_EQUAL_TO.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LET_ITEM_KEY).fullyQualifiedName(TinkarTerm.LET_ITEM_KEY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LINEAGE_FOCUS).fullyQualifiedName(TinkarTerm.LINEAGE_FOCUS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LITERAL_VALUE).fullyQualifiedName(TinkarTerm.LITERAL_VALUE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LITHUANIAN_LANGUAGE).fullyQualifiedName(TinkarTerm.LITHUANIAN_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGIC_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.LOGIC_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGIC_COORDINATE_FOR_TAXONOMY_COORDINATE).fullyQualifiedName(TinkarTerm.LOGIC_COORDINATE_FOR_TAXONOMY_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGIC_COORDINATE_KEY_FOR_MANIFOLD).fullyQualifiedName(TinkarTerm.LOGIC_COORDINATE_KEY_FOR_MANIFOLD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGIC_COORDINATE_NAME).fullyQualifiedName(TinkarTerm.LOGIC_COORDINATE_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGIC_COORDINATE_PROPERTIES).fullyQualifiedName(TinkarTerm.LOGIC_COORDINATE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGIC_DETAILS_PANEL).fullyQualifiedName(TinkarTerm.LOGIC_DETAILS_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGICAL_DEFINITION).fullyQualifiedName(TinkarTerm.LOGICAL_DEFINITION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGICAL_EXPRESSION_FIELD).fullyQualifiedName(TinkarTerm.LOGICAL_EXPRESSION_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC).fullyQualifiedName(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGICAL_FEATURE).fullyQualifiedName(TinkarTerm.LOGICAL_FEATURE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGICALLY_EQUIVALENT_TO).fullyQualifiedName(TinkarTerm.LOGICALLY_EQUIVALENT_TO.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LONG_2).fullyQualifiedName(TinkarTerm.LONG_2.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LONG_FIELD).fullyQualifiedName(TinkarTerm.LONG_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOWER_BOUND).fullyQualifiedName(TinkarTerm.LOWER_BOUND.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MVX_MODULES).fullyQualifiedName(TinkarTerm.MVX_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MANIFOLD_COORDINATE_KEY).fullyQualifiedName(TinkarTerm.MANIFOLD_COORDINATE_KEY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MANIFOLD_COORDINATE_PROPERTIES).fullyQualifiedName(TinkarTerm.MANIFOLD_COORDINATE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MANIFOLD_COORDINATE_REFERENCE).fullyQualifiedName(TinkarTerm.MANIFOLD_COORDINATE_REFERENCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MANIFOLD_FOCUS).fullyQualifiedName(TinkarTerm.MANIFOLD_FOCUS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MANIFOLD_HISTORY).fullyQualifiedName(TinkarTerm.MANIFOLD_HISTORY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MANIFOLD_NAME).fullyQualifiedName(TinkarTerm.MANIFOLD_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MANIFOLD_SELECTION).fullyQualifiedName(TinkarTerm.MANIFOLD_SELECTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAP_PATHWAY_ID).fullyQualifiedName(TinkarTerm.MAP_PATHWAY_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAPPING_DISPLAY_FIELDS).fullyQualifiedName(TinkarTerm.MAPPING_DISPLAY_FIELDS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAPPING_NID_EXTENSION).fullyQualifiedName(TinkarTerm.MAPPING_NID_EXTENSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAPPING_PURPOSE).fullyQualifiedName(TinkarTerm.MAPPING_PURPOSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAPPING_SEMANTIC_TYPE).fullyQualifiedName(TinkarTerm.MAPPING_SEMANTIC_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAPPING_STRING_EXTENSION).fullyQualifiedName(TinkarTerm.MAPPING_STRING_EXTENSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAPS_TO_CODE).fullyQualifiedName(TinkarTerm.MAPS_TO_CODE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAPS_TO_NAME).fullyQualifiedName(TinkarTerm.MAPS_TO_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MARKED_PARENT).fullyQualifiedName(TinkarTerm.MARKED_PARENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MASS_MEASUREMENT_SEMANTIC).fullyQualifiedName(TinkarTerm.MASS_MEASUREMENT_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MASTER_PATH).fullyQualifiedName(TinkarTerm.MASTER_PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MATERNAL_ANCESTOR_OF_SUBJECT_OF_RECORD).fullyQualifiedName(TinkarTerm.MATERNAL_ANCESTOR_OF_SUBJECT_OF_RECORD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MEANING).fullyQualifiedName(TinkarTerm.MEANING.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MEASURE_NARRITIVE).fullyQualifiedName(TinkarTerm.MEASURE_NARRITIVE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MEASURE_PROPERTIES).fullyQualifiedName(TinkarTerm.MEASURE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MEASURE_SEMANTIC).fullyQualifiedName(TinkarTerm.MEASURE_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MEASUREMENT_SEMANTIC).fullyQualifiedName(TinkarTerm.MEASUREMENT_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MEDICATION).fullyQualifiedName(TinkarTerm.MEDICATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MEMBERSHIP_SEMANTIC).fullyQualifiedName(TinkarTerm.MEMBERSHIP_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.METADATA_MODULES).fullyQualifiedName(TinkarTerm.METADATA_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MILIMETERS_OF_MERCURY).fullyQualifiedName(TinkarTerm.MILIMETERS_OF_MERCURY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODE).fullyQualifiedName(TinkarTerm.MODE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODEL_CONCEPT).fullyQualifiedName(TinkarTerm.MODEL_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULE).fullyQualifiedName(TinkarTerm.MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.MODULE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE).fullyQualifiedName(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULE_FOR_USER).fullyQualifiedName(TinkarTerm.MODULE_FOR_USER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULE_FOR_VERSION).fullyQualifiedName(TinkarTerm.MODULE_FOR_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE).fullyQualifiedName(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE).fullyQualifiedName(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE).fullyQualifiedName(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE).fullyQualifiedName(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODULES_FOR_STAMP_COORDINATE).fullyQualifiedName(TinkarTerm.MODULES_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MOTHER_OF_SUBJECT_OF_RECORD).fullyQualifiedName(TinkarTerm.MOTHER_OF_SUBJECT_OF_RECORD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NCBI_GENE_ID).fullyQualifiedName(TinkarTerm.NCBI_GENE_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NDC_CODES_AVAILABLE).fullyQualifiedName(TinkarTerm.NDC_CODES_AVAILABLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NUCC_MODULES).fullyQualifiedName(TinkarTerm.NUCC_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NAME).fullyQualifiedName(TinkarTerm.NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NARROW_TO_BROAD).fullyQualifiedName(TinkarTerm.NARROW_TO_BROAD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NATIVE_ID_FOR_COMPONENT).fullyQualifiedName(TinkarTerm.NATIVE_ID_FOR_COMPONENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NAVIGATION).fullyQualifiedName(TinkarTerm.NAVIGATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NAVIGATION_CONCEPT_SET).fullyQualifiedName(TinkarTerm.NAVIGATION_CONCEPT_SET.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NAVIGATION_PATTERN).fullyQualifiedName(TinkarTerm.NAVIGATION_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NAVIGATION_VERTEX).fullyQualifiedName(TinkarTerm.NAVIGATION_VERTEX.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION).fullyQualifiedName(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NECESSARY_SET).fullyQualifiedName(TinkarTerm.NECESSARY_SET.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NEXT_PRIORITY_LANGUAGE_COORDINATE).fullyQualifiedName(TinkarTerm.NEXT_PRIORITY_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NODE_OPERATOR).fullyQualifiedName(TinkarTerm.NODE_OPERATOR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NORMAL_MEMBER).fullyQualifiedName(TinkarTerm.NORMAL_MEMBER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NORMAL_RANGE).fullyQualifiedName(TinkarTerm.NORMAL_RANGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NOT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.NOT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NOT_APPLICABLE).fullyQualifiedName(TinkarTerm.NOT_APPLICABLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.OID).fullyQualifiedName(TinkarTerm.OID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.OBJECT).fullyQualifiedName(TinkarTerm.OBJECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.OBJECT_PROPERTIES).fullyQualifiedName(TinkarTerm.OBJECT_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.OBSERVATION).fullyQualifiedName(TinkarTerm.OBSERVATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.OR).fullyQualifiedName(TinkarTerm.OR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS).fullyQualifiedName(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS).fullyQualifiedName(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS).fullyQualifiedName(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ORGANISM).fullyQualifiedName(TinkarTerm.ORGANISM.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ORIGIN_FILTER_COORDINATE_KEY_FOR_MANIFOLD).fullyQualifiedName(TinkarTerm.ORIGIN_FILTER_COORDINATE_KEY_FOR_MANIFOLD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PART_OF).fullyQualifiedName(TinkarTerm.PART_OF.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PARTIAL).fullyQualifiedName(TinkarTerm.PARTIAL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PARTICIPANT_ID).fullyQualifiedName(TinkarTerm.PARTICIPANT_ID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PARTICIPANT_PROPERTIES).fullyQualifiedName(TinkarTerm.PARTICIPANT_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PARTICIPANT_ROLE).fullyQualifiedName(TinkarTerm.PARTICIPANT_ROLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PARTICIPANTS).fullyQualifiedName(TinkarTerm.PARTICIPANTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATERNAL_ANCESTOR_OF_SUBJECT_OF_RECORD).fullyQualifiedName(TinkarTerm.PATERNAL_ANCESTOR_OF_SUBJECT_OF_RECORD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH).fullyQualifiedName(TinkarTerm.PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_CONCEPT).fullyQualifiedName(TinkarTerm.PATH_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_COORDINATE_NAME).fullyQualifiedName(TinkarTerm.PATH_COORDINATE_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_COORDINATE_PROPERTIES).fullyQualifiedName(TinkarTerm.PATH_COORDINATE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_FOR_PATH_COORDINATE).fullyQualifiedName(TinkarTerm.PATH_FOR_PATH_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_FOR_STAMP_POSITION).fullyQualifiedName(TinkarTerm.PATH_FOR_STAMP_POSITION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_FOR_USER).fullyQualifiedName(TinkarTerm.PATH_FOR_USER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_FOR_VERSION).fullyQualifiedName(TinkarTerm.PATH_FOR_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE).fullyQualifiedName(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_ORIGINS).fullyQualifiedName(TinkarTerm.PATH_ORIGINS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_ORIGINS_PATTERN).fullyQualifiedName(TinkarTerm.PATH_ORIGINS_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH).fullyQualifiedName(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATH_PRECEDENCE).fullyQualifiedName(TinkarTerm.PATH_PRECEDENCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PATHS_PATTERN).fullyQualifiedName(TinkarTerm.PATHS_PATTERN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PERFORMANCE_CIRCUMSTANCE_PROPERTIES).fullyQualifiedName(TinkarTerm.PERFORMANCE_CIRCUMSTANCE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PERFORMANCE_STATEMENT).fullyQualifiedName(TinkarTerm.PERFORMANCE_STATEMENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PERIOD_DURATION).fullyQualifiedName(TinkarTerm.PERIOD_DURATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PERIOD_START).fullyQualifiedName(TinkarTerm.PERIOD_START.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PERSONA_INSTANCE_NAME).fullyQualifiedName(TinkarTerm.PERSONA_INSTANCE_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PERSONA_NAME).fullyQualifiedName(TinkarTerm.PERSONA_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PERSONA_PROPERTIES).fullyQualifiedName(TinkarTerm.PERSONA_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PHENOMENON).fullyQualifiedName(TinkarTerm.PHENOMENON.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.POLISH_DIALECT).fullyQualifiedName(TinkarTerm.POLISH_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.POLISH_LANGUAGE).fullyQualifiedName(TinkarTerm.POLISH_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.POLYMORPHIC).fullyQualifiedName(TinkarTerm.POLYMORPHIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.POLYMORPHIC_FIELD).fullyQualifiedName(TinkarTerm.POLYMORPHIC_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.POSITION_ON_PATH).fullyQualifiedName(TinkarTerm.POSITION_ON_PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PRECEDENCE).fullyQualifiedName(TinkarTerm.PRECEDENCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PREFERRED).fullyQualifiedName(TinkarTerm.PREFERRED.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PREFERRED_NAME_FOR_CONCEPT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.PREFERRED_NAME_FOR_CONCEPT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PREMISE_TYPE_FOR_MANIFOLD).fullyQualifiedName(TinkarTerm.PREMISE_TYPE_FOR_MANIFOLD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PREMISE_TYPE_FOR_TAXONOMY_COORDINATE).fullyQualifiedName(TinkarTerm.PREMISE_TYPE_FOR_TAXONOMY_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PRESCRIBABLE).fullyQualifiedName(TinkarTerm.PRESCRIBABLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PRESENTATION_UNIT_DIFFERENT).fullyQualifiedName(TinkarTerm.PRESENTATION_UNIT_DIFFERENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PRESSURE_MEASURE_SEMANTIC).fullyQualifiedName(TinkarTerm.PRESSURE_MEASURE_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE).fullyQualifiedName(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PRIMORDIAL_MODULE).fullyQualifiedName(TinkarTerm.PRIMORDIAL_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PRIMORDIAL_PATH).fullyQualifiedName(TinkarTerm.PRIMORDIAL_PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PRIMORDIAL_STATE).fullyQualifiedName(TinkarTerm.PRIMORDIAL_STATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PRIORITY).fullyQualifiedName(TinkarTerm.PRIORITY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PROCEDURE).fullyQualifiedName(TinkarTerm.PROCEDURE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PROMOTION_DESTINATION_PATH).fullyQualifiedName(TinkarTerm.PROMOTION_DESTINATION_PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PROMOTION_PATH_FOR_EDIT_CORDINATE).fullyQualifiedName(TinkarTerm.PROMOTION_PATH_FOR_EDIT_CORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PROMOTION_SOURCE_PATH).fullyQualifiedName(TinkarTerm.PROMOTION_SOURCE_PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PROPERTY_PATTERN_IMPLICATION).fullyQualifiedName(TinkarTerm.PROPERTY_PATTERN_IMPLICATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PROPERTY_SET).fullyQualifiedName(TinkarTerm.PROPERTY_SET.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PROVIDER_CLASS_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.PROVIDER_CLASS_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.PURPOSE).fullyQualifiedName(TinkarTerm.PURPOSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.QUALITY_ASSURANCE_RULE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.QUALITY_ASSURANCE_RULE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.QUALITY_ASSURANCE_RULE_ISSUE).fullyQualifiedName(TinkarTerm.QUALITY_ASSURANCE_RULE_ISSUE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.QUALITY_ASSURANCE_RULE_ISSUE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.QUALITY_ASSURANCE_RULE_ISSUE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.QUERY_CLAUSE_PARAMETERS).fullyQualifiedName(TinkarTerm.QUERY_CLAUSE_PARAMETERS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.QUERY_CLAUSES).fullyQualifiedName(TinkarTerm.QUERY_CLAUSES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.QUERY_STRING).fullyQualifiedName(TinkarTerm.QUERY_STRING.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.QUERY_STRING_IS_REGEX).fullyQualifiedName(TinkarTerm.QUERY_STRING_IS_REGEX.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RF2_INFERRED_RELATIONSHIP_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.RF2_INFERRED_RELATIONSHIP_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RF2_LEGACY_RELATIONSHIP_IMPLICATION_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.RF2_LEGACY_RELATIONSHIP_IMPLICATION_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RF2_STATED_RELATIONSHIP_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.RF2_STATED_RELATIONSHIP_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_IS____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_IS____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_IS_NOT_ACTIVE____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_IS_NOT_ACTIVE____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_IS_NOT_KIND_OF____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_IS_NOT_KIND_OF____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_IS_NOT_MEMBER_OF____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_IS_NOT_MEMBER_OF____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_IS_ACTIVE____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_IS_ACTIVE____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_IS_KIND_OF____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_IS_KIND_OF____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_IS_MEMBER_OF____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_IS_MEMBER_OF____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION).fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFLECTION_CLASS_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.REFLECTION_CLASS_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REFLEXIVE_FEATURE).fullyQualifiedName(TinkarTerm.REFLEXIVE_FEATURE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE).fullyQualifiedName(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RELATIONSHIP_DESTINATION).fullyQualifiedName(TinkarTerm.RELATIONSHIP_DESTINATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RELATIONSHIP_GROUP_FOR_RF2_RELATIONSHIP).fullyQualifiedName(TinkarTerm.RELATIONSHIP_GROUP_FOR_RF2_RELATIONSHIP.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RELATIONSHIP_IS_CIRCULAR____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.RELATIONSHIP_IS_CIRCULAR____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RELATIONSHIP_ORIGIN).fullyQualifiedName(TinkarTerm.RELATIONSHIP_ORIGIN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RELATIONSHIP_RESTRICTION____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.RELATIONSHIP_RESTRICTION____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RELATIONSHIP_TYPE____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.RELATIONSHIP_TYPE____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RELATIONSHIP_TYPE_IN_SOURCE_TERMINOLOGY).fullyQualifiedName(TinkarTerm.RELATIONSHIP_TYPE_IN_SOURCE_TERMINOLOGY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REPETITION_PROPERTIES).fullyQualifiedName(TinkarTerm.REPETITION_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REPETITIONS).fullyQualifiedName(TinkarTerm.REPETITIONS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REPLACEMENT_CONCEPT).fullyQualifiedName(TinkarTerm.REPLACEMENT_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REPRESENTS_ASSOCIATION).fullyQualifiedName(TinkarTerm.REPRESENTS_ASSOCIATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REQUEST_PRIORITY).fullyQualifiedName(TinkarTerm.REQUEST_PRIORITY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REQUEST_CIRCUMSTANCE_PROPERTIES).fullyQualifiedName(TinkarTerm.REQUEST_CIRCUMSTANCE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REQUEST_STATEMENT).fullyQualifiedName(TinkarTerm.REQUEST_STATEMENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REQUESTED_PARTICIPANTS).fullyQualifiedName(TinkarTerm.REQUESTED_PARTICIPANTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.REQUESTED_RESULT).fullyQualifiedName(TinkarTerm.REQUESTED_RESULT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RESOLUTION).fullyQualifiedName(TinkarTerm.RESOLUTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RESULT).fullyQualifiedName(TinkarTerm.RESULT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RESULT_PROPERTIES).fullyQualifiedName(TinkarTerm.RESULT_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RIGHT_PANE_DEFAULTS).fullyQualifiedName(TinkarTerm.RIGHT_PANE_DEFAULTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RIGHT_PANE_OPTIONS).fullyQualifiedName(TinkarTerm.RIGHT_PANE_OPTIONS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RIGHT_TAB_NODES).fullyQualifiedName(TinkarTerm.RIGHT_TAB_NODES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ROLE).fullyQualifiedName(TinkarTerm.ROLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ROLE_GROUP).fullyQualifiedName(TinkarTerm.ROLE_GROUP.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ROLE_OPERATOR).fullyQualifiedName(TinkarTerm.ROLE_OPERATOR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ROLE_TYPE).fullyQualifiedName(TinkarTerm.ROLE_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ROLE_TYPE_TO_ADD).fullyQualifiedName(TinkarTerm.ROLE_TYPE_TO_ADD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE).fullyQualifiedName(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.ROUTINE).fullyQualifiedName(TinkarTerm.ROUTINE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RULE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.RULE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RUSSIAN_DIALECT).fullyQualifiedName(TinkarTerm.RUSSIAN_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RUSSIAN_LANGUAGE).fullyQualifiedName(TinkarTerm.RUSSIAN_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RXNORM_ASSEMBLAGES).fullyQualifiedName(TinkarTerm.RXNORM_ASSEMBLAGES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RXNORM_ASSERTED).fullyQualifiedName(TinkarTerm.RXNORM_ASSERTED.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RXNORM_CUI).fullyQualifiedName(TinkarTerm.RXNORM_CUI.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RXNORM_CONCEPT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.RXNORM_CONCEPT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RXNORM_INFERRED).fullyQualifiedName(TinkarTerm.RXNORM_INFERRED.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RXNORM_ISSUE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.RXNORM_ISSUE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RXNORM_LICENSE).fullyQualifiedName(TinkarTerm.RXNORM_LICENSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.RXNORM_MODULES).fullyQualifiedName(TinkarTerm.RXNORM_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SCTID).fullyQualifiedName(TinkarTerm.SCTID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SH_PROFILE).fullyQualifiedName(TinkarTerm.SH_PROFILE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SKOS_ALTERNATE_LABEL).fullyQualifiedName(TinkarTerm.SKOS_ALTERNATE_LABEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SKOS_DEFINITION).fullyQualifiedName(TinkarTerm.SKOS_DEFINITION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SKOS_PREFERRED_LABEL).fullyQualifiedName(TinkarTerm.SKOS_PREFERRED_LABEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SNOMED_CT_CORE_MODULES).fullyQualifiedName(TinkarTerm.SNOMED_CT_CORE_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SNOMED_AFFILIATES_LICENSE).fullyQualifiedName(TinkarTerm.SNOMED_AFFILIATES_LICENSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SNOMED_ISSUE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.SNOMED_ISSUE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_AUTOMATION_RULE_MODULE).fullyQualifiedName(TinkarTerm.SOLOR_AUTOMATION_RULE_MODULE.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.SOLOR_CONCEPT).fullyQualifiedName(TinkarTerm.SOLOR_CONCEPT.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.SOLOR_CONCEPT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.SOLOR_CONCEPT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_ISSUE_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.SOLOR_ISSUE_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_MODULE).fullyQualifiedName(TinkarTerm.SOLOR_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_OVERLAY_MODULE).fullyQualifiedName(TinkarTerm.SOLOR_OVERLAY_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_QUALITY_ASSURANCE_RULE_MODULE).fullyQualifiedName(TinkarTerm.SOLOR_QUALITY_ASSURANCE_RULE_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_TEMPORARY_CONCEPT_MODULE).fullyQualifiedName(TinkarTerm.SOLOR_TEMPORARY_CONCEPT_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOPT_MODULES).fullyQualifiedName(TinkarTerm.SOPT_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SRF_INFERRED_RELATIONSHIP_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.SRF_INFERRED_RELATIONSHIP_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SRF_LEGACY_RELATIONSHIP_IMPLICATION_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.SRF_LEGACY_RELATIONSHIP_IMPLICATION_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SRF_STATED_RELATIONSHIP_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.SRF_STATED_RELATIONSHIP_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SANDBOX_COMPONENT).fullyQualifiedName(TinkarTerm.SANDBOX_COMPONENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SANDBOX_MODULE).fullyQualifiedName(TinkarTerm.SANDBOX_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SANDBOX_PATH).fullyQualifiedName(TinkarTerm.SANDBOX_PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SANDBOX_PATH_MODULE).fullyQualifiedName(TinkarTerm.SANDBOX_PATH_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEMANTIC_FIELD_CONCEPTS).fullyQualifiedName(TinkarTerm.SEMANTIC_FIELD_CONCEPTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEMANTIC_FIELD_DATA_TYPES_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.SEMANTIC_FIELD_DATA_TYPES_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEMANTIC_FIELD_NAME).fullyQualifiedName(TinkarTerm.SEMANTIC_FIELD_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEMANTIC_FIELD_TYPE).fullyQualifiedName(TinkarTerm.SEMANTIC_FIELD_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEMANTIC_FIELDS_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.SEMANTIC_FIELDS_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEMANTIC_PROPERTIES).fullyQualifiedName(TinkarTerm.SEMANTIC_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEMANTIC_TREE_TABLE_PANEL).fullyQualifiedName(TinkarTerm.SEMANTIC_TREE_TABLE_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEMANTIC_TYPE).fullyQualifiedName(TinkarTerm.SEMANTIC_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEQUENCE).fullyQualifiedName(TinkarTerm.SEQUENCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SIGNED_INTEGER).fullyQualifiedName(TinkarTerm.SIGNED_INTEGER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SIGNIFY_A_CONCEPT_IN_WRITING).fullyQualifiedName(TinkarTerm.SIGNIFY_A_CONCEPT_IN_WRITING.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SIMPLE_SEARCH_PANEL).fullyQualifiedName(TinkarTerm.SIMPLE_SEARCH_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SKIN_OF_REGION_TEMPLATE).fullyQualifiedName(TinkarTerm.SKIN_OF_REGION_TEMPLATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SNOROCKET_CLASSIFIER).fullyQualifiedName(TinkarTerm.SNOROCKET_CLASSIFIER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_LIVD_MODULE).fullyQualifiedName(TinkarTerm.SOLOR_LIVD_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_UMLS_MODULE).fullyQualifiedName(TinkarTerm.SOLOR_UMLS_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_FOUNDATION_MODULE).fullyQualifiedName(TinkarTerm.SOLOR_FOUNDATION_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOLOR_GENOMIC_MODULE).fullyQualifiedName(TinkarTerm.SOLOR_GENOMIC_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOURCE_ARTIFACT_VERSION).fullyQualifiedName(TinkarTerm.SOURCE_ARTIFACT_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOURCE_CODE_SYSTEM).fullyQualifiedName(TinkarTerm.SOURCE_CODE_SYSTEM.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOURCE_CODE_VERSION).fullyQualifiedName(TinkarTerm.SOURCE_CODE_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOURCE_CONTENT_VERSION).fullyQualifiedName(TinkarTerm.SOURCE_CONTENT_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SOURCE_RELEASE_DATE).fullyQualifiedName(TinkarTerm.SOURCE_RELEASE_DATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SPANISH_DIALECT_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.SPANISH_DIALECT_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SPANISH_LANGUAGE).fullyQualifiedName(TinkarTerm.SPANISH_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SPECIMEN).fullyQualifiedName(TinkarTerm.SPECIMEN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STAMP_FILTER_FOR_PATH).fullyQualifiedName(TinkarTerm.STAMP_FILTER_FOR_PATH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STANDARD_KOREAN_DIALECT).fullyQualifiedName(TinkarTerm.STANDARD_KOREAN_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE).fullyQualifiedName(TinkarTerm.STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.STATED_NAVIGATION).fullyQualifiedName(TinkarTerm.STATED_NAVIGATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATED_PREMISE_TYPE).fullyQualifiedName(TinkarTerm.STATED_PREMISE_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_ACTION_TOPIC).fullyQualifiedName(TinkarTerm.STATEMENT_ACTION_TOPIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_ASSOCIATION_PROPERTIES).fullyQualifiedName(TinkarTerm.STATEMENT_ASSOCIATION_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_ASSOCIATIONS).fullyQualifiedName(TinkarTerm.STATEMENT_ASSOCIATIONS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_AUTHORS).fullyQualifiedName(TinkarTerm.STATEMENT_AUTHORS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_CIRCUMSTANCE).fullyQualifiedName(TinkarTerm.STATEMENT_CIRCUMSTANCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_IDENTIFIER).fullyQualifiedName(TinkarTerm.STATEMENT_IDENTIFIER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_MODE).fullyQualifiedName(TinkarTerm.STATEMENT_MODE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_NARRATIVE).fullyQualifiedName(TinkarTerm.STATEMENT_NARRATIVE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_PROPERTIES).fullyQualifiedName(TinkarTerm.STATEMENT_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_SUBJECT_OF_INFORMATION).fullyQualifiedName(TinkarTerm.STATEMENT_SUBJECT_OF_INFORMATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_SUBJECT_OF_RECORD).fullyQualifiedName(TinkarTerm.STATEMENT_SUBJECT_OF_RECORD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_TIME).fullyQualifiedName(TinkarTerm.STATEMENT_TIME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATEMENT_TYPE).fullyQualifiedName(TinkarTerm.STATEMENT_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATUS_FOR_VERSION).fullyQualifiedName(TinkarTerm.STATUS_FOR_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STATUS_VALUE).fullyQualifiedName(TinkarTerm.STATUS_VALUE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING).fullyQualifiedName(TinkarTerm.STRING.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_1).fullyQualifiedName(TinkarTerm.STRING_1.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_2).fullyQualifiedName(TinkarTerm.STRING_2.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_3).fullyQualifiedName(TinkarTerm.STRING_3.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_4).fullyQualifiedName(TinkarTerm.STRING_4.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_5).fullyQualifiedName(TinkarTerm.STRING_5.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_6).fullyQualifiedName(TinkarTerm.STRING_6.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_7).fullyQualifiedName(TinkarTerm.STRING_7.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_FIELD).fullyQualifiedName(TinkarTerm.STRING_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_FOR_SEMANTIC).fullyQualifiedName(TinkarTerm.STRING_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_LITERAL).fullyQualifiedName(TinkarTerm.STRING_LITERAL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_SEMANTIC).fullyQualifiedName(TinkarTerm.STRING_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SUBJECT_OF_INFORMATION).fullyQualifiedName(TinkarTerm.SUBJECT_OF_INFORMATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SUBJECT_OF_RECORD).fullyQualifiedName(TinkarTerm.SUBJECT_OF_RECORD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SUBSTANCE).fullyQualifiedName(TinkarTerm.SUBSTANCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SUBSTANCE_DOES_NOT_EXIST).fullyQualifiedName(TinkarTerm.SUBSTANCE_DOES_NOT_EXIST.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION).fullyQualifiedName(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR).fullyQualifiedName(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SUFFICIENT_SET).fullyQualifiedName(TinkarTerm.SUFFICIENT_SET.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SWEDISH_LANGUAGE).fullyQualifiedName(TinkarTerm.SWEDISH_LANGUAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SYMMETRIC_FEATURE).fullyQualifiedName(TinkarTerm.SYMMETRIC_FEATURE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SYNCHRONIZATION_ITEM_PROPERTIES).fullyQualifiedName(TinkarTerm.SYNCHRONIZATION_ITEM_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SYSTEM_DASHBOARD_PANEL).fullyQualifiedName(TinkarTerm.SYSTEM_DASHBOARD_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TARGET_CODE_SYSTEM).fullyQualifiedName(TinkarTerm.TARGET_CODE_SYSTEM.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TARGET_CODE_VERSION).fullyQualifiedName(TinkarTerm.TARGET_CODE_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TARGET_TERMINOLOGY_DATE).fullyQualifiedName(TinkarTerm.TARGET_TERMINOLOGY_DATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TAXONOMY_CONFIGURATION_NAME).fullyQualifiedName(TinkarTerm.TAXONOMY_CONFIGURATION_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TAXONOMY_CONFIGURATION_PROPERTIES).fullyQualifiedName(TinkarTerm.TAXONOMY_CONFIGURATION_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TAXONOMY_CONFIGURATION_ROOTS).fullyQualifiedName(TinkarTerm.TAXONOMY_CONFIGURATION_ROOTS.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TAXONOMY_OPERATOR).fullyQualifiedName(TinkarTerm.TAXONOMY_OPERATOR.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TAXONOMY_PANEL).fullyQualifiedName(TinkarTerm.TAXONOMY_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TEMPLATE_CONCEPT).fullyQualifiedName(TinkarTerm.TEMPLATE_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TEMPLATE_MERGE).fullyQualifiedName(TinkarTerm.TEMPLATE_MERGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TEMPLATE_MODE).fullyQualifiedName(TinkarTerm.TEMPLATE_MODE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TEST_MODULE).fullyQualifiedName(TinkarTerm.TEST_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TEST_PROMOTION_MODULE).fullyQualifiedName(TinkarTerm.TEST_PROMOTION_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TEXT_COMPARISON_MEASURE_SEMANTIC).fullyQualifiedName(TinkarTerm.TEXT_COMPARISON_MEASURE_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TEXT_FOR_DESCRIPTION).fullyQualifiedName(TinkarTerm.TEXT_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TIME_FOR_VERSION).fullyQualifiedName(TinkarTerm.TIME_FOR_VERSION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TIME_MEASUREMENT_SEMANTIC).fullyQualifiedName(TinkarTerm.TIME_MEASUREMENT_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TIME_PRECEDENCE).fullyQualifiedName(TinkarTerm.TIME_PRECEDENCE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TIMING).fullyQualifiedName(TinkarTerm.TIMING.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TRANSACTION_LIST_PANEL).fullyQualifiedName(TinkarTerm.TRANSACTION_LIST_PANEL.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TRANSITIVE_FEATURE).fullyQualifiedName(TinkarTerm.TRANSITIVE_FEATURE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TREE_AMALGAM_PROPERTIES).fullyQualifiedName(TinkarTerm.TREE_AMALGAM_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TREE_LIST).fullyQualifiedName(TinkarTerm.TREE_LIST.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TYPE).fullyQualifiedName(TinkarTerm.TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TYPE_NID_FOR_RF2_RELATIONSHIP).fullyQualifiedName(TinkarTerm.TYPE_NID_FOR_RF2_RELATIONSHIP.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TYPE_OF_STATEMENT).fullyQualifiedName(TinkarTerm.TYPE_OF_STATEMENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UMLS_EQUIVALENCY_ASSEMBLAGE).fullyQualifiedName(TinkarTerm.UMLS_EQUIVALENCY_ASSEMBLAGE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.US_ENGLISH_DIALECT).fullyQualifiedName(TinkarTerm.US_ENGLISH_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.US_EXTENSION_MODULES).fullyQualifiedName(TinkarTerm.US_EXTENSION_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.US_GOVERNMENT_WORK).fullyQualifiedName(TinkarTerm.US_GOVERNMENT_WORK.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.US_NURSING_DIALECT).fullyQualifiedName(TinkarTerm.US_NURSING_DIALECT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UUID_DATA_TYPE).fullyQualifiedName(TinkarTerm.UUID_DATA_TYPE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UUID_FIELD).fullyQualifiedName(TinkarTerm.UUID_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UUID_FOR_TAXONOMY_COORDINATE).fullyQualifiedName(TinkarTerm.UUID_FOR_TAXONOMY_COORDINATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UUID_LIST_FOR_COMPONENT).fullyQualifiedName(TinkarTerm.UUID_LIST_FOR_COMPONENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNCATEGORIZED_PHENOMENON).fullyQualifiedName(TinkarTerm.UNCATEGORIZED_PHENOMENON.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNICODE_EVALUATION).fullyQualifiedName(TinkarTerm.UNICODE_EVALUATION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNINITIALIZED_COMPONENT).fullyQualifiedName(TinkarTerm.UNINITIALIZED_COMPONENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNITS_DIFFERENT).fullyQualifiedName(TinkarTerm.UNITS_DIFFERENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNIVERSAL_RESTRICTION).fullyQualifiedName(TinkarTerm.UNIVERSAL_RESTRICTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER).fullyQualifiedName(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNMAPPABLE).fullyQualifiedName(TinkarTerm.UNMAPPABLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNMODELED_CONCEPT).fullyQualifiedName(TinkarTerm.UNMODELED_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNMODELED_FEATURE_CONCEPT).fullyQualifiedName(TinkarTerm.UNMODELED_FEATURE_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNMODELED_ROLE_CONCEPT).fullyQualifiedName(TinkarTerm.UNMODELED_ROLE_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNMODELED_TAXONOMIC_CONCEPT).fullyQualifiedName(TinkarTerm.UNMODELED_TAXONOMIC_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNSTRUCTURED_CIRCUMSTANCE_PROPERTIES).fullyQualifiedName(TinkarTerm.UNSTRUCTURED_CIRCUMSTANCE_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UNSTRUCTURED_CIRCUMSTANCE_TEXT).fullyQualifiedName(TinkarTerm.UNSTRUCTURED_CIRCUMSTANCE_TEXT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.UPPER_BOUND).fullyQualifiedName(TinkarTerm.UPPER_BOUND.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.USER).fullyQualifiedName(TinkarTerm.USER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VA_STATION_IEN).fullyQualifiedName(TinkarTerm.VA_STATION_IEN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VA_STATION_NUMBER).fullyQualifiedName(TinkarTerm.VA_STATION_NUMBER.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VHAT_MODULES).fullyQualifiedName(TinkarTerm.VHAT_MODULES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VUID).fullyQualifiedName(TinkarTerm.VUID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VACCINE).fullyQualifiedName(TinkarTerm.VACCINE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VALUE).fullyQualifiedName(TinkarTerm.VALUE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VALUES_DIFFERENT).fullyQualifiedName(TinkarTerm.VALUES_DIFFERENT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VERSION_LIST_FOR_CHRONICLE).fullyQualifiedName(TinkarTerm.VERSION_LIST_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VERSION_PROPERTIES).fullyQualifiedName(TinkarTerm.VERSION_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VERSION_TYPE_FOR_ACTION).fullyQualifiedName(TinkarTerm.VERSION_TYPE_FOR_ACTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VERTEX_STAMP_FILTER_FOR_MANIFOLD).fullyQualifiedName(TinkarTerm.VERTEX_STAMP_FILTER_FOR_MANIFOLD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VERTEX_FIELD).fullyQualifiedName(TinkarTerm.VERTEX_FIELD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VERTEX_SORT).fullyQualifiedName(TinkarTerm.VERTEX_SORT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VERTEX_STATE_SET).fullyQualifiedName(TinkarTerm.VERTEX_STATE_SET.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VETERINARY_MEDICINE_ONLY).fullyQualifiedName(TinkarTerm.VETERINARY_MEDICINE_ONLY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VIEW_STAMP_FILTER_FOR_MANIFOLD).fullyQualifiedName(TinkarTerm.VIEW_STAMP_FILTER_FOR_MANIFOLD.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.VIEW_COORDINATE_KEY).fullyQualifiedName(TinkarTerm.VIEW_COORDINATE_KEY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.WINDOW_CONFIGURATION_NAME).fullyQualifiedName(TinkarTerm.WINDOW_CONFIGURATION_NAME.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.WINDOW_CONFIGURATION_PROPERTIES).fullyQualifiedName(TinkarTerm.WINDOW_CONFIGURATION_PROPERTIES.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.WINDOW_HEIGHT).fullyQualifiedName(TinkarTerm.WINDOW_HEIGHT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.WINDOW_WIDTH).fullyQualifiedName(TinkarTerm.WINDOW_WIDTH.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.WINDOW_X_POSITION).fullyQualifiedName(TinkarTerm.WINDOW_X_POSITION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.WINDOW_Y_POSITION).fullyQualifiedName(TinkarTerm.WINDOW_Y_POSITION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.WITHDRAWN_STATE).fullyQualifiedName(TinkarTerm.WITHDRAWN_STATE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.XOR____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.XOR____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.AND_NOT____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.AND_NOT____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BOOLEAN).fullyQualifiedName(TinkarTerm.BOOLEAN.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.BYTE_ARRAY).fullyQualifiedName(TinkarTerm.BYTE_ARRAY.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.CHANGED_BETWEEN_STAMPS____QUERY_CLAUSE).fullyQualifiedName(TinkarTerm.CHANGED_BETWEEN_STAMPS____QUERY_CLAUSE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT).fullyQualifiedName(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.DOUBLE).fullyQualifiedName(TinkarTerm.DOUBLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.FLOAT).fullyQualifiedName(TinkarTerm.FLOAT.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC).fullyQualifiedName(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LONG).fullyQualifiedName(TinkarTerm.LONG.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.LONG_VALUE_FOR_SEMANTIC).fullyQualifiedName(TinkarTerm.LONG_VALUE_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAPPING_METADATA).fullyQualifiedName(TinkarTerm.MAPPING_METADATA.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MAPPING_NAMESPACE).fullyQualifiedName(TinkarTerm.MAPPING_NAMESPACE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MILLIGRAM).fullyQualifiedName(TinkarTerm.MILLIGRAM.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.MODIFIER_NID_FOR_RF2_RELATIONSHIP).fullyQualifiedName(TinkarTerm.MODIFIER_NID_FOR_RF2_RELATIONSHIP.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.NID).fullyQualifiedName(TinkarTerm.NID.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE).fullyQualifiedName(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.STRING_SUBSTITUTION).fullyQualifiedName(TinkarTerm.STRING_SUBSTITUTION.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.TARGET).fullyQualifiedName(TinkarTerm.TARGET.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.USERS_MODULE).fullyQualifiedName(TinkarTerm.USERS_MODULE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.C_INFORMATICS_INCORPORATED).fullyQualifiedName(TinkarTerm.C_INFORMATICS_INCORPORATED.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.C_REGENSTRIEF_INSTITUTE_INC_AND_C_THE_LOGICAL_OBSERVATION_IDENTIFIERS_NAMES_AND_CODES_LOINC_COMMITTEE).fullyQualifiedName(TinkarTerm.C_REGENSTRIEF_INSTITUTE_INC_AND_C_THE_LOGICAL_OBSERVATION_IDENTIFIERS_NAMES_AND_CODES_LOINC_COMMITTEE.description(), TinkarTerm.PREFERRED).build();
        starterData.concept(TinkarTerm.C_SNOMED_INTERNATIONAL).fullyQualifiedName(TinkarTerm.C_SNOMED_INTERNATIONAL.description(), TinkarTerm.PREFERRED).build();
//        starterData.concept(TinkarTerm.ROOT_VERTEX).fullyQualifiedName(TinkarTerm.ROOT_VERTEX.description(), TinkarTerm.PREFERRED).build();


        starterData.build();
        starterData.shutdown();
    }
}
