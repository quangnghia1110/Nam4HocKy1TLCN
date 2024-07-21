package studentConsulting.model.entity.news;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

@Data
@Builder
@Entity
@Table(name = "news_images")
@NoArgsConstructor
@AllArgsConstructor
public class NewsImageEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã hình ảnh

    @ManyToOne
    @JoinColumn(name = "news_id", nullable = false, referencedColumnName = "id")
    private NewsEntity news; // Mã bài đăng tham chiếu

    @Column(name = "image_url", nullable = false, length = 1000)
    private String imageUrl; // Liên kết đến hình ảnh
}
