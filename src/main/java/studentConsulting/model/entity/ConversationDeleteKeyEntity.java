package studentConsulting.model.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Embeddable;
import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Embeddable
public class ConversationDeleteKeyEntity implements Serializable {
    private Integer conversationId;
    private Integer userId;


}
