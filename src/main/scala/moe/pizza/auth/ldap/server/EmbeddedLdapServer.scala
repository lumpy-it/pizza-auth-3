package moe.pizza.auth.ldap.server

import java.io.{File, IOException}
import java.util
import java.util.UUID
import javax.naming.NamingException

import net.sf.ehcache.Cache
import org.apache.directory.api.ldap.model.constants.SchemaConstants
import org.apache.directory.api.ldap.model.csn.CsnFactory
import org.apache.directory.api.ldap.model.entry._
import org.apache.directory.api.ldap.model.exception.{
  LdapInvalidDnException,
  LdapException
}
import org.apache.directory.api.ldap.model.name.Dn
import org.apache.directory.api.ldap.model.schema.{SchemaManager, SchemaObject}
import org.apache.directory.api.ldap.model.schema.parsers.OpenLdapSchemaParser
import org.apache.directory.api.ldap.schema.extractor.impl.DefaultSchemaLdifExtractor
import org.apache.directory.server.core.DefaultDirectoryService
import org.apache.directory.server.core.api.entry.ClonedServerEntry
import org.apache.directory.server.core.api.interceptor.context.{
  HasEntryOperationContext,
  AddOperationContext
}
import org.apache.directory.server.core.api.partition.Partition
import org.apache.directory.server.core.api.{
  CoreSession,
  InstanceLayout,
  DirectoryService
}
import org.apache.directory.server.core.factory.DefaultDirectoryServiceFactory
import org.apache.directory.server.core.partition.impl.avl.AvlPartition
import org.apache.directory.server.core.partition.impl.btree.jdbm.JdbmPartition
import org.apache.directory.server.core.partition.ldif.LdifPartition
import org.apache.directory.server.core.shared.DefaultDnFactory
import org.apache.directory.server.ldap.LdapServer
import org.apache.directory.server.protocol.shared.store.LdifFileLoader
import org.apache.directory.server.protocol.shared.transport.TcpTransport
import org.apache.directory.server.xdbm.impl.avl.AvlIndex
import org.slf4j.{LoggerFactory, Logger}

class EmbeddedLdapServer(instancePath: String,
                         basedn: String,
                         host: String,
                         port: Int,
                         instanceName: String = "pizza-auth-ldap") {
  private final val log: Logger = LoggerFactory.getLogger(getClass)
  var directoryService: DirectoryService = null
  var ldapService: LdapServer = null

  private val schemaParser = new OpenLdapSchemaParser
  val pizzaSchema = schemaParser.parse(
    scala.io.Source
      .fromInputStream(getClass.getResourceAsStream(
        "/schemas/" + basedn.replace("ou=", "") + ".schema"))
      .getLines()
      .mkString("\n"))

  init

  def setPassword(password: String,
                  uid: String = "uid=admin,ou=system"): Unit = {
    val setpassword = new DefaultModification(
      ModificationOperation.REPLACE_ATTRIBUTE,
      "userPassword",
      password)
    directoryService.getAdminSession.modify(new Dn(uid), setpassword)
  }

  def createOu(adminsession: CoreSession) = {

    /*
    createEntry("ou=pizza", Map(
      "objectClass" -> "top",
      "objectClass" -> "extensibleObject",
      "objectClass" -> "organizationalUnit",
      "ou" -> "pizza"
    ))
     */

    createEntry(s"cn=admin,$basedn",
                Map(
                  "objectClass" -> "account",
                  "objectClass" -> "top",
                  "objectClass" -> "simpleSecurityObject",
                  "objectClass" -> "organizationalPerson",
                  "sn" -> "Admin",
                  "cn" -> "admin"
                ))
  }

  def createEntry(dn: String, name: String, sm: SchemaManager): Entry = {
    val e = new DefaultEntry(sm,
                             dn,
                             SchemaConstants.ENTRY_CSN_AT,
                             new CsnFactory(0).newInstance().toString,
                             SchemaConstants.ENTRY_UUID_AT,
                             UUID.randomUUID().toString)
    e.put("objectClass", "top", "organizationalUnit")
    e.put("ou", name)
    new ClonedServerEntry(e)
  }

  def loadSchemas(adminsession: CoreSession) = {
    val ll = new LdifFileLoader(
      adminsession,
      "src/main/resources/schemasds/" + basedn.replace("ou=", "") + ".ldif")
    val r = ll.execute()
    log.debug("attempted to insert schemas, result :%d".format(r))
  }

  @throws(classOf[Exception])
  @throws(classOf[IOException])
  @throws(classOf[LdapException])
  @throws(classOf[NamingException])
  private def init() {
    val factory: DefaultDirectoryServiceFactory =
      new DefaultDirectoryServiceFactory
    factory.init(instanceName)
    directoryService = factory.getDirectoryService
    directoryService.getChangeLog.setEnabled(false)
    directoryService.setShutdownHookEnabled(true)
    val il: InstanceLayout = new InstanceLayout(instancePath)
    directoryService.setInstanceLayout(il)

    val partition: LdifPartition = new LdifPartition(
      directoryService.getSchemaManager,
      directoryService.getDnFactory)
    partition.setId(instanceName)
    partition.setPartitionPath(
      new File(directoryService.getInstanceLayout.getPartitionsDirectory,
               basedn.replace("ou=", "")).toURI)
    partition.setSchemaManager(directoryService.getSchemaManager)
    partition.setSuffixDn(new Dn(basedn))
    partition.setCacheService(directoryService.getCacheService)

    val partitionroot = createEntry(basedn,
                                    basedn.replace("ou=", ""),
                                    directoryService.getSchemaManager)

    loadSchemas(directoryService.getAdminSession)
    directoryService.addPartition(partition)
    directoryService.getPartitions
    ldapService = new LdapServer
    ldapService.setTransports(new TcpTransport(host, port))
    ldapService.setDirectoryService(directoryService)
    partition.initialize
    if (!partition.hasEntry(
          new HasEntryOperationContext(directoryService.getAdminSession,
                                       new Dn(basedn)))) {
      partition.add(
        new AddOperationContext(directoryService.getAdminSession,
                                partitionroot))
    }
    log.debug(
      "partition was set up for %s".format(partition.getSuffixDn.toString))
  }

  def start() {
    directoryService.startup
    ldapService.start
    createOu(directoryService.getAdminSession)
  }

  def stop() {
    ldapService.stop
    directoryService.shutdown
  }

  def createEntry(id: String, attributes: Map[String, String]) {
    val dn: Dn = new Dn(directoryService.getSchemaManager, id)
    if (!directoryService.getAdminSession.exists(dn)) {
      val entry: Entry = directoryService.newEntry(dn)
      for (attributeId <- attributes.keySet) {
        entry.add(attributeId, attributes.get(attributeId).get)
      }
      directoryService.getAdminSession.add(entry)
    }
  }

  def createEntry(id: String, e: Entry): Unit = {
    if (!directoryService.getAdminSession.exists(id)) {
      directoryService.getAdminSession.add(e)
    }

  }
}
