package studentConsulting.model.entity.communication;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.io.Serializable;

@Data
@Builder
@Entity
@Table(name = "conversation_user")
@NoArgsConstructor
@AllArgsConstructor
public class ConversationUserEntity implements Serializable {

    @EmbeddedId
    private ConversationUserKey id;

    @ManyToOne
    @MapsId("conversationId")  
    @JoinColumn(name = "conversation_id")
    private ConversationEntity conversation;

    @ManyToOne
    @MapsId("userId")  
    @JoinColumn(name = "user_id")
    private UserInformationEntity user;

}
