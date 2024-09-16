package studentConsulting.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.MessageEntity;

@Repository
public interface MessageRepository extends JpaRepository<MessageEntity, Integer> {
    List<MessageEntity> findByConversationId(Integer conversationId);
    
    @Modifying
    @Query("DELETE FROM MessageEntity m WHERE m.conversation = :conversation")
    void deleteMessagesByConversation(@Param("conversation") ConversationEntity conversation);
}
