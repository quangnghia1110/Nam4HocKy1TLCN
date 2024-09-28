package studentConsulting.model.entity.communication;

import java.io.Serializable;
import javax.persistence.Embeddable;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Embeddable
public class ConversationUserKeyEntity implements Serializable {

    private Integer conversationId;
    private Integer userId;
}
