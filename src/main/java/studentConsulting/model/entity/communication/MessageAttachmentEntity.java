package studentConsulting.model.entity.communication;

import java.sql.Timestamp;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@Entity
@Table(name = "message_attachments")
@NoArgsConstructor
@AllArgsConstructor
public class MessageAttachmentEntity{
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
    @ManyToOne
    @JoinColumn(name = "message_id", nullable = false, referencedColumnName = "id")
    private MessageEntity message; // Mã tin nhắn

    @Column(name = "file_name", nullable = false, length = 255)
    private String fileName; // Tên tệp đính kèm

    @Column(name = "file_type", nullable = false, length = 50)
    private String fileType; // Loại tệp đính kèm

    @Column(name = "file_size", nullable = false)
    private Integer fileSize; // Kích thước tệp đính kèm

    @Column(name = "file_url", nullable = false, length = 1000)
    private String fileUrl; // Liên kết đến tệp đính kèm
}

