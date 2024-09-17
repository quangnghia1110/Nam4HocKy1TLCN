package studentConsulting.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;
import studentConsulting.model.entity.communication.ConversationUserKey;

@Repository
public interface ConversationUserRepository extends JpaRepository<ConversationUserEntity, ConversationUserKey> {
    boolean existsByConversationAndUser(ConversationEntity conversation, UserInformationEntity user);

    @Modifying
    @Query("DELETE FROM ConversationUserEntity cu WHERE cu.conversation = :conversation")
    void deleteMembersByConversation(@Param("conversation") ConversationEntity conversation);

    @Modifying
    @Query("DELETE FROM ConversationUserEntity cu WHERE cu.conversation = :conversation AND cu.user = :user")
    void deleteByConversationAndUser(@Param("conversation") ConversationEntity conversation, @Param("user") UserInformationEntity user);
    
    @Query("SELECT cu FROM ConversationUserEntity cu WHERE cu.conversation.id = :conversationId")
    List<ConversationUserEntity> findByConversationId(@Param("conversationId") Integer conversationId);
    Optional<ConversationUserEntity> findByConversation_IdAndUser_Id(Integer conversationId, Integer userId);

    @Query("SELECT cu.conversation FROM ConversationUserEntity cu WHERE cu.user.id = :userId OR cu.conversation.user.id = :userId")
    List<ConversationEntity> findConversationsByUserId(@Param("userId") Integer userId);
    boolean existsByConversation_IdAndUser_Id(Integer conversationId, Integer userId);

}