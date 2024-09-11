package studentConsulting.model.entity.news;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.sql.Timestamp;

import javax.persistence.*;

@Data
@Builder
@Entity
@Table(name = "news_attachments")
@NoArgsConstructor
@AllArgsConstructor
public class NewsAttachmentEntity{
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
    @ManyToOne
    @JoinColumn(name = "news_id", nullable = false, referencedColumnName = "id")
    private NewsEntity news; 

    @Column(name = "file_name", nullable = false, length = 255)
    private String fileName; 

    @Column(name = "file_type", nullable = false, length = 50)
    private String fileType; 

    @Column(name = "file_size", nullable = false)
    private Integer fileSize;

    @Column(name = "file_url", nullable = false, length = 1000)
    private String fileUrl; 
}
