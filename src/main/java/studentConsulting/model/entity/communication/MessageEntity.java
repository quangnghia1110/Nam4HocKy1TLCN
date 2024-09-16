package studentConsulting.model.entity.communication;

import java.time.LocalDateTime;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.constant.enums.MessageStatus;

@Data
@Builder
@Entity
@Table(name = "messages")
@NoArgsConstructor
@AllArgsConstructor
public class MessageEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @ManyToOne
    @JoinColumn(name = "conversation_id")
    @JsonIgnore
    private ConversationEntity conversation;
    
    private String senderName;
    private String receiverName;
    
    private String message;
    private LocalDateTime date;

    @Enumerated(EnumType.STRING)
    private MessageStatus messageStatus;
}

