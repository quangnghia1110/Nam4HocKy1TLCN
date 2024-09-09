package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.communication.ConversationEntity;

@Repository
public interface ConversationRepository extends JpaRepository<ConversationEntity, Integer> {

    @Query("SELECT c FROM ConversationEntity c WHERE CONCAT(c.user.lastName, c.user.firstName) = :fullName1 AND CONCAT(c.consultant.lastName, c.consultant.firstName) = :fullName2 OR CONCAT(c.user.lastName, c.user.firstName) = :fullName2 AND CONCAT(c.consultant.lastName, c.consultant.firstName) = :fullName1")
    ConversationEntity findByUserAndConsultantFullName(@Param("fullName1") String fullName1, @Param("fullName2") String fullName2);

}

