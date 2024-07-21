package studentConsulting.model.entity.news;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

@Data
@Builder
@Entity
@Table(name = "news_attachments")
@NoArgsConstructor
@AllArgsConstructor
public class NewsAttachmentEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã tệp đính kèm

    @ManyToOne
    @JoinColumn(name = "news_id", nullable = false, referencedColumnName = "id")
    private NewsEntity news; // Mã bài đăng tham chiếu

    @Column(name = "file_name", nullable = false, length = 255)
    private String fileName; // Tên tệp đính kèm

    @Column(name = "file_type", nullable = false, length = 50)
    private String fileType; // Loại tệp đính kèm (ví dụ: pdf, docx, xlsx)

    @Column(name = "file_size", nullable = false)
    private Integer fileSize; // Kích thước tệp đính kèm (theo byte)

    @Column(name = "file_url", nullable = false, length = 1000)
    private String fileUrl; // Liên kết đến tệp đính kèm (URL hoặc đường dẫn lưu trữ)
}
