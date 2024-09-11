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
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; 

    @Column(name = "title", nullable = false, length = 255, unique = true)
    private String title;

    @Column(name = "content", nullable = false, length = 500)
    private String content; 

    @Column(name = "view", nullable = false)
    private Integer view; 

    @Column(name = "status_approval", nullable = false)
    private Boolean statusApproval; 

    @Column(name = "status_delete", nullable = false)
    private Boolean statusDelete; 

    @OneToMany(mappedBy = "news", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NewsAttachmentEntity> attachments; 

    @OneToMany(mappedBy = "news", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NewsImageEntity> images; 

    @OneToMany(mappedBy = "news", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NewsShareEntity> shares; 
}
