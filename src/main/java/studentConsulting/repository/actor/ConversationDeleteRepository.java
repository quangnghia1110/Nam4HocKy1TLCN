package studentConsulting.repository.actor;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.*;

import java.util.List;
import java.util.Optional;

@Repository
public interface ConversationDeleteRepository extends JpaRepository<ConversationDeleteEntity, ConversationDeleteKeyEntity> {
    @Modifying
    @Query("DELETE FROM ConversationDeleteEntity cde WHERE cde.conversation = :conversation")
    void deleteMembersByConversation(@Param("conversation") ConversationEntity conversation);

    @Modifying
    @Query("DELETE FROM ConversationDeleteEntity cde WHERE cde.conversation = :conversation AND cde.user.id = :userId")
    void deleteMembersByConversationAndUserId(@Param("conversation") ConversationEntity conversation,@Param("userId") Integer userId);

    @Query("SELECT COUNT(cde) FROM ConversationDeleteEntity cde WHERE cde.conversation.id = :conversationId")
    long countUsersDeleted(@Param("conversationId") Integer conversationId);

    @Query("SELECT cde FROM ConversationDeleteEntity cde WHERE cde.conversation.id = :conversationId AND cde.user.id = :userId")
    Optional<ConversationDeleteEntity> findByConversationIdAndUserId(
            @Param("conversationId") Integer conversationId,
            @Param("userId") Integer userId);

    boolean existsByConversationIdAndUserId(Integer conversationId, Integer userId);

    @Modifying
    @Query("DELETE FROM ConversationDeleteEntity cd WHERE cd.conversation.id = :conversationId AND cd.user.id = :userId")
    void deleteByConversationIdAndUserId(@Param("conversationId") Integer conversationId, @Param("userId") Integer userId);
}
