package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.user.UserInformationEntity;

import java.util.Optional;

@Repository
public interface ConversationRepository extends PagingAndSortingRepository<ConversationEntity, Integer>, JpaSpecificationExecutor<ConversationEntity> {

    @Query("SELECT c FROM ConversationEntity c WHERE CONCAT(c.user.lastName, c.user.firstName) = :fullName1 AND CONCAT(c.consultant.lastName, c.consultant.firstName) = :fullName2 OR CONCAT(c.user.lastName, c.user.firstName) = :fullName2 AND CONCAT(c.consultant.lastName, c.consultant.firstName) = :fullName1")
    ConversationEntity findByUserAndConsultantFullName(@Param("fullName1") String fullName1, @Param("fullName2") String fullName2);

    @Modifying
    @Query("DELETE FROM ConversationEntity c WHERE c = :conversation")
    void deleteConversation(@Param("conversation") ConversationEntity conversation);

    boolean existsByIdAndUser_Id(Integer conversationId, Integer userId);

    @Query("SELECT c FROM ConversationEntity c WHERE c.user = :user AND c.consultant = :consultant")
    Optional<ConversationEntity> findByUserAndConsultant(@Param("user") UserInformationEntity user, @Param("consultant") UserInformationEntity consultant);

    boolean existsByUserAndConsultantAndDepartment(UserInformationEntity user, UserInformationEntity consultant, DepartmentEntity department);

}

