package studentConsulting.model.entity.feedback;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import javax.persistence.*;
import java.sql.Timestamp;

@Data
@Builder
@Entity
@Table(name = "ratings")
@NoArgsConstructor
@AllArgsConstructor
public class RatingEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã rating

    @ManyToOne
    @JoinColumn(name = "review_id", nullable = false, referencedColumnName = "id")
    private ReviewEntity review; // Mã đánh giá tham chiếu

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @Column(name = "rating_value", nullable = false)
    private Integer ratingValue; // Giá trị rating

    @Column(name = "created_at", nullable = false, updatable = false, insertable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt; // Thời gian tạo rating

    @Column(name = "updated_at", nullable = false, insertable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt; // Thời gian cập nhật
}

