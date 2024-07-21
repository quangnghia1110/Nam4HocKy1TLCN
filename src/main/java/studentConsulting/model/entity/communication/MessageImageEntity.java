package studentConsulting.model.entity.communication;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@Entity
@Table(name = "messages_images")
@NoArgsConstructor
@AllArgsConstructor
public class MessageImageEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã hình ảnh

    @ManyToOne
    @JoinColumn(name = "messages_id", nullable = false, referencedColumnName = "id")
    private MessageEntity message; // Mã tin nhắn

    @Column(name = "link_image", nullable = false, length = 1000)
    private String linkImage; // Liên kết đến hình ảnh
}
