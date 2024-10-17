open module tinkar.sandbox {
    requires dev.ikm.tinkar.terms;
    requires dev.ikm.tinkar.schema;
    requires dev.ikm.tinkar.provider.ephemeral;
    requires dev.ikm.tinkar.provider.spinedarray;
    requires dev.ikm.tinkar.provider.entity;
    requires dev.ikm.tinkar.provider.executor;
    requires dev.ikm.tinkar.provider.search;
    requires dev.ikm.tinkar.coordinate;
    requires dev.ikm.jpms.eclipse.collections.api;
    requires java.logging;
    requires com.fasterxml.jackson.databind;
    requires com.fasterxml.uuid;
    requires org.slf4j;
    requires dev.ikm.tinkar.composer;

    exports dev.ikm.tinkar.starterdata;

}