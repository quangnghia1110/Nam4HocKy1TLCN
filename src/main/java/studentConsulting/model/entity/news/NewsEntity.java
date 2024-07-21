package studentConsulting.model.entity.news;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import javax.persistence.*;
import java.sql.Timestamp;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "news")
@NoArgsConstructor
@AllArgsConstructor
public class NewsEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã bài đăng

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @Column(name = "title", nullable = false, length = 255, unique = true)
    private String title; // Tiêu đề

    @Column(name = "content", nullable = false, length = 500)
    private String content; // Nội dung

    @Column(name = "view", nullable = false)
    private Integer view; // Số view của bài đăng

    @Column(name = "status_approval", nullable = false)
    private Boolean statusApproval; // Đã được kiểm duyệt, chưa được kiểm duyệt

    @Column(name = "status_delete", nullable = false)
    private Boolean statusDelete; // Đã xóa, chưa xóa

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt; // Thời gian tạo bài đăng

    @Column(name = "updated_at", nullable = false, insertable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt; // Thời gian cập nhật

    @OneToMany(mappedBy = "news", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NewsAttachmentEntity> attachments; // Các tệp đính kèm của bài đăng

    @OneToMany(mappedBy = "news", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NewsImageEntity> images; // Các hình ảnh của bài đăng

    @OneToMany(mappedBy = "news", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NewsShareEntity> shares; // Các chia sẻ của bài đăng
}
